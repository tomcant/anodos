use crate::attacks::{bishop_attacks, king_attacks, knight_attacks, pawn_attacks, rook_attacks};
use crate::colour::Colour;
use crate::eval::terms::PIECE_WEIGHTS;
use crate::r#move::Move;
use crate::piece::Piece::{self, *};
use crate::position::Board;
use crate::square::Square;
use lazy_static::lazy_static;

pub fn see_ge(board: &Board, mv: &Move) -> bool {
    debug_assert!(mv.captured_piece.is_some());

    // Simulate the first exchange and return early if we are ahead or equal.
    let mut balance = piece_weight(mv.captured_piece.unwrap());
    balance -= piece_weight(mv.promotion_piece.unwrap_or(mv.piece));

    if balance >= 0 {
        return true;
    }

    let mut colour_to_move = mv.piece.colour().flip();
    let mut occupancy = board.occupancy() ^ mv.from.u64();

    if mv.is_en_passant {
        occupancy ^= mv.capture_square().unwrap().u64();
    }

    let knights = board.pieces(WN) | board.pieces(BN);
    let bishops = board.pieces(WB) | board.pieces(BB);
    let rooks = board.pieces(WR) | board.pieces(BR);
    let queens = board.pieces(WQ) | board.pieces(BQ);
    let kings = board.pieces(WK) | board.pieces(BK);

    let mut attackers = occupancy
        & ((pawn_attacks(mv.to, Colour::White) & board.pieces(BP))
            | (pawn_attacks(mv.to, Colour::Black) & board.pieces(WP))
            | (knight_attacks(mv.to) & knights)
            | (bishop_attacks(mv.to, occupancy) & (bishops | queens))
            | (rook_attacks(mv.to, occupancy) & (rooks | queens))
            | (king_attacks(mv.to) & kings));

    loop {
        let Some((piece, square)) = least_valuable_attacker(colour_to_move, board, attackers) else {
            break;
        };

        if piece.is_king() && attackers & board.pieces_by_colour(colour_to_move.flip()) != 0 {
            break;
        }

        colour_to_move = colour_to_move.flip();
        balance = -balance - piece_weight(piece);

        if piece.is_pawn() && mv.to.is_back_rank() {
            balance -= piece_weight(WQ) - piece_weight(WP);
        }

        if balance >= 0 {
            break;
        }

        occupancy ^= square.u64();

        // Update attackers in case the moving piece had a bishop/rook/queen behind it.
        if matches!(piece, WP | BP | WB | BB | WQ | BQ) {
            attackers |= bishop_attacks(mv.to, occupancy) & (bishops | queens);
        }
        if matches!(piece, WR | BR | WQ | BQ) {
            attackers |= rook_attacks(mv.to, occupancy) & (rooks | queens);
        }

        attackers &= occupancy;
    }

    mv.piece.colour() != colour_to_move
}

fn least_valuable_attacker(colour: Colour, board: &Board, attackers: u64) -> Option<(Piece, Square)> {
    let our_attackers = attackers & board.pieces_by_colour(colour);

    if our_attackers == 0 {
        return None;
    }

    for piece in [
        Piece::pawn(colour),
        Piece::knight(colour),
        Piece::bishop(colour),
        Piece::rook(colour),
        Piece::queen(colour),
        Piece::king(colour),
    ] {
        let attackers = our_attackers & board.pieces(piece);

        if attackers != 0 {
            return Some((piece, Square::first(attackers)));
        }
    }

    None
}

#[inline]
fn piece_weight(piece: Piece) -> i32 {
    SEE_PIECE_WEIGHTS[piece]
}

lazy_static! {
    // SEE piece weights are based on the material weights but the king must be
    // worth the most so that `see_ge()` doesn't return true for captures that
    // leave the side to move in check.
    static ref SEE_PIECE_WEIGHTS: [i32; 12] = {
        let mut weights = PIECE_WEIGHTS;
        weights[WK] = weights[WQ] + 1;
        weights[BK] = weights[BQ] + 1;
        weights
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::square::Square::*;
    use crate::testing::*;

    #[test]
    fn good_captures() {
        #[rustfmt::skip]
        let cases = [
            ("4R3/2r3p1/5bk1/1p1r3p/p2PR1P1/P1BK1P2/1P6/8 b - -", H5, G4),
            ("4R3/2r3p1/5bk1/1p1r1p1p/p2PR1P1/P1BK1P2/1P6/8 b - -", H5, G4),
            ("2r1r1k1/pp1bppbp/3p1np1/q3P3/2P2P2/1P2B3/P1N1B1PP/2RQ1RK1 b - -", D6, E5),
            ("8/4kp2/2npp3/1Nn5/1p2PQP1/7q/1PP1B3/4KR1r b - -", H1, F1),
            ("2r2r1k/6bp/p7/2q2p1Q/3PpP2/1B6/P5PP/2RR3K b - -", C5, C1),
            ("r2qk1nr/pp2ppbp/2b3p1/2p1p3/8/2N2N2/PPPP1PPP/R1BQR1K1 w kq -", F3, E5),
            ("6r1/4kq2/b2p1p2/p1pPb3/p1P2B1Q/2P4P/2B1R1P1/6K1 w - -", F4, E5),
            ("2r4r/1P4pk/p2p1b1p/7n/BB3p2/2R2p2/P1P2P2/4RK2 w - -", C3, C8),
            ("2r4k/2r4p/p7/2b2p1b/4pP2/1BR5/P1R3PP/2Q4K w - -", C3, C5),
            ("4q3/1p1pr1kb/1B2rp2/6p1/p3PP2/P3R1P1/1P2R1K1/4Q3 b - -", H7, E4),
            ("7k/P7/6r1/1p1q4/1QP2N2/3b3n/PP4P1/4K3 w - -", B4, B5),
        ];
        for (fen, from, to) in cases {
            let pos = parse_fen(fen);
            let piece = pos.board.piece_at(from).unwrap();
            let captured_piece = pos.board.piece_at(to).unwrap();
            let mv = make_move(piece, from, to, Some(captured_piece));
            assert!(see_ge(&pos.board, &mv));
        }

        #[rustfmt::skip]
        let en_passant_cases = [
            ("3q2nk/pb1r1p2/np6/3P2Pp/2p1P3/2R4B/PQ3P1P/3R2K1 w - h6", G5, H6),
            ("3q2nk/pb1r1p2/np6/3P2Pp/2p1P3/2R1B2B/PQ3P1P/3R2K1 w - h6", G5, H6),
        ];
        for (fen, from, to) in en_passant_cases {
            let mv = make_en_passant_move(Colour::White, from, to);
            assert!(see_ge(&parse_fen(fen).board, &mv));
        }
    }

    #[test]
    fn bad_captures() {
        #[rustfmt::skip]
        let cases = [
            ("4k3/6q1/5b2/4p3/8/2B5/1Q6/4K3 w - -", C3, E5),
            ("3qk3/3r4/3r4/8/3P4/3R4/3R4/3QK3 b - -", D6, D4),
            ("4k3/5p2/4p3/8/2B5/1Q6/8/4K3 w - -", C4, E6),
            ("4k3/5p2/4p3/8/8/4R3/4R3/4K3 w - -", E3, E6),
            ("4r1k1/5pp1/nbp4p/1p2p2q/1P2P1b1/1BP2N1P/1B2QPPK/3R4 b - -", G4, F3),
        ];
        for (fen, from, to) in cases {
            let pos = parse_fen(fen);
            let piece = pos.board.piece_at(from).unwrap();
            let captured_piece = pos.board.piece_at(to).unwrap();
            assert!(!see_ge(&pos.board, &make_move(piece, from, to, Some(captured_piece))));
        }
    }
}
