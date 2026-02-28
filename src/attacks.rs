use crate::colour::Colour;
use crate::piece::Piece::{self, *};
use crate::position::Board;
use crate::square::{FILES, Square};
use lazy_static::lazy_static;

// Include build-generated magic tables
include!(concat!(env!("OUT_DIR"), "/magic.rs"));

#[inline]
pub fn is_in_check(colour: Colour, board: &Board) -> bool {
    let king_square = Square::first(board.pieces(Piece::king(colour)));

    is_attacked(king_square, colour.flip(), board)
}

#[inline]
pub fn is_attacked(square: Square, colour: Colour, board: &Board) -> bool {
    let bishop_attacks = bishop_attacks(square, board.occupancy());
    let rook_attacks = rook_attacks(square, board.occupancy());
    let queen_attacks = bishop_attacks | rook_attacks;

    #[rustfmt::skip]
    let attackers =
          (board.pieces(Piece::pawn(colour)) & pawn_attacks(square, colour.flip()))
        | (board.pieces(Piece::knight(colour)) & knight_attacks(square))
        | (board.pieces(Piece::bishop(colour)) & bishop_attacks)
        | (board.pieces(Piece::rook(colour)) & rook_attacks)
        | (board.pieces(Piece::queen(colour)) & queen_attacks)
        | (board.pieces(Piece::king(colour)) & king_attacks(square));

    attackers != 0
}

#[inline]
pub fn attacks(piece: Piece, square: Square, board: &Board) -> u64 {
    match piece {
        WP | BP => pawn_attacks(square, piece.colour()) & board.pieces_by_colour(piece.colour().flip()),
        WN | BN => knight_attacks(square),
        WB | BB => bishop_attacks(square, board.occupancy()),
        WR | BR => rook_attacks(square, board.occupancy()),
        WQ | BQ => bishop_attacks(square, board.occupancy()) | rook_attacks(square, board.occupancy()),
        WK | BK => king_attacks(square),
    }
}

#[inline]
pub fn en_passant_attacks(en_passant_square: Square, colour: Colour, board: &Board) -> u64 {
    PAWN_ATTACKS[colour.flip()][en_passant_square] & board.pieces(Piece::pawn(colour))
}

#[inline]
pub fn pawn_attacks(square: Square, colour: Colour) -> u64 {
    PAWN_ATTACKS[colour][square]
}

#[inline]
pub fn knight_attacks(square: Square) -> u64 {
    KNIGHT_ATTACKS[square]
}

#[inline]
pub fn bishop_attacks(square: Square, occupancy: u64) -> u64 {
    let magic = &BISHOP_MAGICS[square];
    let masked = occupancy & magic.mask;
    let index = ((masked.wrapping_mul(magic.num)) >> magic.shift) as usize;

    BISHOP_ATTACKS[magic.offset + index]
}

#[inline]
pub fn rook_attacks(square: Square, occupancy: u64) -> u64 {
    let magic = &ROOK_MAGICS[square];
    let masked = occupancy & magic.mask;
    let index = ((masked.wrapping_mul(magic.num)) >> magic.shift) as usize;

    ROOK_ATTACKS[magic.offset + index]
}

#[inline]
pub fn king_attacks(square: Square) -> u64 {
    KING_ATTACKS[square]
}

lazy_static! {
    static ref SQUARES: [Square; 64] = (0..64).map(Square::from_index).collect::<Vec<_>>().try_into().unwrap();

    static ref PAWN_ATTACKS: [[u64; 64]; 2] = {
        let mut attacks = [[0; 64]; 2];

        for square in SQUARES.iter() {
            let square_u64 = square.u64();

            attacks[Colour::White][*square] =
                  (square_u64 & !FILES[0]) << 7 | (square_u64 & !FILES[7]) << 9;

            attacks[Colour::Black][*square] =
                  (square_u64 & !FILES[7]) >> 7 | (square_u64 & !FILES[0]) >> 9;
        }

        attacks
    };

    static ref KNIGHT_ATTACKS: [u64; 64] = {
        let mut attacks = [0; 64];

        for square in SQUARES.iter() {
            let square_u64 = square.u64();

            attacks[*square] =
                  (square_u64 & !FILES[0] & !FILES[1]) << 6  // up 1, left 2
                | (square_u64 & !FILES[6] & !FILES[7]) << 10 // up 1, right 2
                | (square_u64 & !FILES[0]) << 15             // up 2, left 1
                | (square_u64 & !FILES[7]) << 17             // up 2, right 1

                | (square_u64 & !FILES[6] & !FILES[7]) >> 6  // down 1, right 2
                | (square_u64 & !FILES[0] & !FILES[1]) >> 10 // down 1, left 2
                | (square_u64 & !FILES[7]) >> 15             // down 2, right 1
                | (square_u64 & !FILES[0]) >> 17;            // down 2, left 1
        }

        attacks
    };

    static ref KING_ATTACKS: [u64; 64] = {
        let mut attacks = [0; 64];

        for square in SQUARES.iter() {
            let square_u64 = square.u64();

            attacks[*square] =
                  (square_u64 & !FILES[7]) << 1
                | (square_u64 & !FILES[0]) >> 1

                | square_u64 << 8
                | (square_u64 & !FILES[0]) << 7
                | (square_u64 & !FILES[7]) << 9

                | square_u64 >> 8
                | (square_u64 & !FILES[7]) >> 7
                | (square_u64 & !FILES[0]) >> 9;
        }

        attacks
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::position::Position;
    use crate::square::Square::*;
    use crate::testing::*;

    #[test]
    fn detect_check() {
        let mut board = Board::empty();
        board.put_piece(BK, E8);
        board.put_piece(WN, D6);

        assert!(is_in_check(Colour::Black, &board));
    }

    #[test]
    fn attack_by_queen_horizontal() {
        let pos = parse_fen("Q3k3/8/8/8/8/8/8/8 w - -");

        assert!(is_attacked(E8, Colour::White, &pos.board));
    }

    #[test]
    fn attack_by_queen_vertical() {
        let pos = parse_fen("4k3/8/8/8/4Q3/8/8/8 w - -");

        assert!(is_attacked(E8, Colour::White, &pos.board));
    }

    #[test]
    fn attack_by_queen_diagonal() {
        let pos = parse_fen("4k3/8/8/8/Q7/8/8/8 w - -");

        assert!(is_attacked(E8, Colour::White, &pos.board));
    }

    #[test]
    fn white_pawn_attacks_none() {
        let pos = parse_fen("8/8/8/8/8/8/4P3/8 w - -");

        assert_attacks_eq(&pos, E2, &[]);
    }

    #[test]
    fn white_pawn_attacks_left() {
        let pos = parse_fen("8/8/8/8/8/3p4/4P3/8 w - -");

        assert_attacks_eq(&pos, E2, &[D3]);
    }

    #[test]
    fn white_pawn_attacks_right() {
        let pos = parse_fen("8/8/8/8/8/5p2/4P3/8 w - -");

        assert_attacks_eq(&pos, E2, &[F3]);
    }

    #[test]
    fn white_pawn_attacks_left_and_right() {
        let pos = parse_fen("8/8/8/8/8/3p1p2/4P3/8 w - -");

        assert_attacks_eq(&pos, E2, &[D3, F3]);
    }

    #[test]
    fn black_pawn_attacks_none() {
        let pos = parse_fen("8/4p3/8/8/8/8/8/8 b - -");

        assert_attacks_eq(&pos, E7, &[]);
    }

    #[test]
    fn black_pawn_attacks_left() {
        let pos = parse_fen("8/4p3/3P4/8/8/8/8/8 b - -");

        assert_attacks_eq(&pos, E7, &[D6]);
    }

    #[test]
    fn black_pawn_attacks_right() {
        let pos = parse_fen("8/4p3/5P2/8/8/8/8/8 b - -");

        assert_attacks_eq(&pos, E7, &[F6]);
    }

    #[test]
    fn black_pawn_attacks_left_and_right() {
        let pos = parse_fen("8/4p3/3P1P2/8/8/8/8/8 b - -");

        assert_attacks_eq(&pos, E7, &[D6, F6]);
    }

    #[test]
    fn knight_attacks() {
        let pos = parse_fen("8/8/8/8/3N4/8/8/8 w - -");

        assert_attacks_eq(&pos, D4, &[C2, E2, B3, F3, B5, F5, C6, E6]);
    }

    #[test]
    fn bishop_attacks_on_empty_board() {
        let pos = parse_fen("8/8/8/8/3b4/8/8/8 b - -");

        assert_attacks_eq(&pos, D4, &[A1, G1, B2, F2, C3, E3, C5, E5, B6, F6, A7, G7, H8]);
    }

    #[test]
    fn bishop_attacks_with_up_left_blocker() {
        let pos = parse_fen("8/8/2n5/8/8/8/6B1/8 w - -");

        assert_attacks_eq(&pos, G2, &[H1, H3, F1, F3, E4, D5, C6]);
    }

    #[test]
    fn bishop_attacks_with_up_right_blocker() {
        let pos = parse_fen("8/8/5n2/8/8/8/1B6/8 w - -");

        assert_attacks_eq(&pos, B2, &[A1, A3, C1, C3, D4, E5, F6]);
    }

    #[test]
    fn bishop_attacks_with_down_left_blocker() {
        let pos = parse_fen("8/8/8/4B3/3n4/8/8/8 w - -");

        assert_attacks_eq(&pos, E5, &[H8, G7, F6, D4, F4, G3, H2, D6, C7, B8]);
    }

    #[test]
    fn bishop_attacks_with_down_right_blocker() {
        let pos = parse_fen("8/8/8/3b4/8/5N2/8/8 w - -");

        assert_attacks_eq(&pos, D5, &[A8, B7, C6, E6, F7, G8, C4, B3, A2, E4, F3]);
    }

    #[test]
    fn rook_attacks_on_empty_board() {
        let pos = parse_fen("8/8/8/8/3r4/8/8/8 b - -");

        assert_attacks_eq(&pos, D4, &[D1, D2, D3, D5, D6, D7, D8, A4, B4, C4, E4, F4, G4, H4]);
    }

    #[test]
    fn rook_attacks_with_up_blocker() {
        let pos = parse_fen("8/8/8/3N4/8/8/8/3r4 b - -");

        assert_attacks_eq(&pos, D1, &[D2, D3, D4, D5, A1, B1, C1, E1, F1, G1, H1]);
    }

    #[test]
    fn rook_attacks_with_right_blocker() {
        let pos = parse_fen("8/8/8/r2N4/8/8/8/8 b - -");

        assert_attacks_eq(&pos, A5, &[B5, C5, D5, A6, A7, A8, A4, A3, A2, A1]);
    }

    #[test]
    fn rook_attacks_with_left_blocker() {
        let pos = parse_fen("8/8/8/3N3r/8/8/8/8 b - -");

        assert_attacks_eq(&pos, H5, &[G5, F5, E5, D5, H6, H7, H8, H4, H3, H2, H1]);
    }

    #[test]
    fn rook_attacks_with_down_blocker() {
        let pos = parse_fen("3r4/8/8/3N4/8/8/8/8 b - -");

        assert_attacks_eq(&pos, D8, &[D7, D6, D5, A8, B8, C8, E8, F8, G8, H8]);
    }

    #[test]
    fn king_attacks() {
        let pos = parse_fen("8/8/8/8/8/8/8/4K3 w - -");

        assert_attacks_eq(&pos, E1, &[D1, F1, D2, E2, F2]);
    }

    fn assert_attacks_eq(pos: &Position, attacker: Square, squares: &[Square]) {
        let expected_attacks: u64 = squares.iter().map(|square| square.u64()).sum();
        let actual_attacks = attacks(pos.board.piece_at(attacker).unwrap(), attacker, &pos.board);

        assert_eq!(expected_attacks, actual_attacks);
    }
}
