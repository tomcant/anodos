use crate::attacks::{attacks, en_passant_attacks, is_attacked, is_in_check};
use crate::colour::Colour;
use crate::r#move::Move;
use crate::piece::Piece;
use crate::position::{Board, CastlingRight, CastlingRights, Position};
use crate::square::{
    BACK_RANKS,
    Square::{self, *},
};
use smallvec::SmallVec;

pub const MAX_MOVES: usize = 128;
pub type MoveList = SmallVec<[Move; MAX_MOVES]>;

const PAWN_START_RANKS: [u8; 2] = [1, 6];

const WHITE_KING_CASTLING_PATH: u64 = F1.u64() | G1.u64();
const BLACK_KING_CASTLING_PATH: u64 = F8.u64() | G8.u64();
const WHITE_QUEEN_CASTLING_PATH: u64 = B1.u64() | C1.u64() | D1.u64();
const BLACK_QUEEN_CASTLING_PATH: u64 = B8.u64() | C8.u64() | D8.u64();

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MoveGenType {
    All,
    Noisy,
    Quiet,
}

impl MoveGenType {
    fn includes_noisy(&self) -> bool {
        matches!(self, Self::All | Self::Noisy)
    }

    fn includes_quiet(&self) -> bool {
        matches!(self, Self::All | Self::Quiet)
    }
}

pub fn generate_all_moves(pos: &Position) -> MoveList {
    generate_moves(pos, MoveGenType::All)
}

pub fn generate_noisy_moves(pos: &Position) -> MoveList {
    generate_moves(pos, MoveGenType::Noisy)
}

pub fn generate_quiet_moves(pos: &Position) -> MoveList {
    generate_moves(pos, MoveGenType::Quiet)
}

#[inline(always)]
fn generate_moves(pos: &Position, gen_type: MoveGenType) -> MoveList {
    let mut moves = MoveList::new();
    let colour_to_move = pos.colour_to_move;
    let board = &pos.board;

    let (targets, pawn_advance_ranks) = match gen_type {
        MoveGenType::All => (!board.pieces_by_colour(colour_to_move), !0),
        MoveGenType::Noisy => (board.pieces_by_colour(colour_to_move.flip()), BACK_RANKS),
        MoveGenType::Quiet => (!board.occupancy(), !BACK_RANKS),
    };

    for piece in Piece::pieces_by_colour(colour_to_move) {
        let mut pieces = board.pieces(*piece);

        while pieces != 0 {
            let from_square = Square::next(&mut pieces);
            let mut to_squares = targets & attacks(*piece, from_square, board);

            if piece.is_pawn() {
                to_squares |= pawn_advances(from_square, colour_to_move, board) & pawn_advance_ranks;
            } else if piece.is_king() && gen_type.includes_quiet() {
                to_squares |= castling(pos.castling_rights, colour_to_move, board);
            }

            while to_squares != 0 {
                let to_square = Square::next(&mut to_squares);
                let captured_piece = board.piece_at(to_square);

                if piece.is_pawn() && to_square.is_back_rank() {
                    for promotion_piece in Piece::promotions(colour_to_move) {
                        moves.push(Move {
                            piece: *piece,
                            from: from_square,
                            to: to_square,
                            captured_piece,
                            promotion_piece: Some(*promotion_piece),
                            is_en_passant: false,
                        });
                    }

                    continue;
                }

                moves.push(Move {
                    piece: *piece,
                    from: from_square,
                    to: to_square,
                    captured_piece,
                    promotion_piece: None,
                    is_en_passant: false,
                });
            }
        }
    }

    if gen_type.includes_noisy()
        && let Some(en_passant_square) = pos.en_passant_square
    {
        let mut from_squares = en_passant_attacks(en_passant_square, colour_to_move, board);

        while from_squares != 0 {
            moves.push(Move {
                piece: Piece::pawn(colour_to_move),
                from: Square::next(&mut from_squares),
                to: en_passant_square,
                captured_piece: Some(Piece::pawn(colour_to_move.flip())),
                promotion_piece: None,
                is_en_passant: true,
            });
        }
    }

    moves
}

pub fn is_pseudo_legal_quiet_move(pos: &Position, mv: &Move) -> bool {
    let colour_to_move = pos.colour_to_move;
    let board = &pos.board;

    if !mv.is_quiet()
        || mv.piece.colour() != colour_to_move
        || board.piece_at(mv.from) != Some(mv.piece)
        || board.has_piece_at(mv.to)
    {
        return false;
    }

    let to_squares = if mv.piece.is_pawn() {
        pawn_advances(mv.from, colour_to_move, board) & !BACK_RANKS
    } else if mv.is_castling() {
        castling(pos.castling_rights, colour_to_move, board)
    } else {
        attacks(mv.piece, mv.from, board)
    };

    to_squares & mv.to.u64() != 0
}

fn pawn_advances(square: Square, colour: Colour, board: &Board) -> u64 {
    let one_ahead = square.advance(colour);

    if board.has_piece_at(one_ahead) {
        return 0;
    }

    if square.rank() != PAWN_START_RANKS[colour] {
        return one_ahead.u64();
    }

    let two_ahead = one_ahead.advance(colour);

    if board.has_piece_at(two_ahead) {
        return one_ahead.u64();
    }

    one_ahead.u64() | two_ahead.u64()
}

fn castling(rights: CastlingRights, colour: Colour, board: &Board) -> u64 {
    let castling = match colour {
        Colour::White => white_castling(rights, board),
        _ => black_castling(rights, board),
    };

    if castling != 0 && !is_in_check(colour, board) {
        return castling;
    }

    0
}

fn white_castling(rights: CastlingRights, board: &Board) -> u64 {
    let mut castling = 0;

    if rights.has(CastlingRight::WhiteKing)
        && !board.has_occupancy_at(WHITE_KING_CASTLING_PATH)
        && !is_attacked(F1, Colour::Black, board)
    {
        castling |= G1.u64();
    }

    if rights.has(CastlingRight::WhiteQueen)
        && !board.has_occupancy_at(WHITE_QUEEN_CASTLING_PATH)
        && !is_attacked(D1, Colour::Black, board)
    {
        castling |= C1.u64();
    }

    castling
}

fn black_castling(rights: CastlingRights, board: &Board) -> u64 {
    let mut castling = 0;

    if rights.has(CastlingRight::BlackKing)
        && !board.has_occupancy_at(BLACK_KING_CASTLING_PATH)
        && !is_attacked(F8, Colour::White, board)
    {
        castling |= G8.u64();
    }

    if rights.has(CastlingRight::BlackQueen)
        && !board.has_occupancy_at(BLACK_QUEEN_CASTLING_PATH)
        && !is_attacked(D8, Colour::White, board)
    {
        castling |= C8.u64();
    }

    castling
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::position::START_POS_FEN;
    use crate::testing::*;

    #[test]
    fn legal_move_count_in_checkmate_is_zero() {
        assert_legal_move_count("rnb1kbnr/pppp1ppp/4p3/8/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq -", 0);
    }

    #[test]
    fn legal_move_count_in_check_is_limited() {
        assert_legal_move_count("rnbqkbnr/1pp1p1pp/p2p1p2/1B6/8/4P3/PPPP1PPP/RNBQK1NR b KQq -", 7);
    }

    #[test]
    fn white_pawn_moves() {
        assert_pseudo_legal_move_count("8/8/8/8/8/8/4P3/8 w - -", 2);
    }

    #[test]
    fn black_pawn_moves() {
        assert_pseudo_legal_move_count("8/4p3/8/8/8/8/8/8 b - -", 2);
    }

    #[test]
    fn single_pawn_advance() {
        assert_pseudo_legal_move_count("8/8/8/8/4p3/8/4P3/8 w - -", 1);
    }

    #[test]
    fn double_pawn_advance() {
        assert_pseudo_legal_move_count("8/8/8/8/8/4p3/4P3/8 w - -", 0);
    }

    #[test]
    fn knight_moves() {
        assert_pseudo_legal_move_count("8/8/8/8/3N4/8/8/8 w - -", 8);
    }

    #[test]
    fn bishop_moves() {
        assert_pseudo_legal_move_count("8/r7/5n2/8/3B4/8/8/8 w - -", 11);
    }

    #[test]
    fn rook_moves() {
        assert_pseudo_legal_move_count("8/3b4/8/8/1n1R4/8/8/8 w - -", 12);
    }

    #[test]
    fn king_moves() {
        assert_pseudo_legal_move_count("8/8/8/8/8/8/8/4K3 w - -", 5);
    }

    #[test]
    fn pawn_promotion_with_advance() {
        assert_pseudo_legal_move_count("8/4P3/8/8/8/8/8/8 w - -", 4);
    }

    #[test]
    fn pawn_promotion_with_capture() {
        assert_pseudo_legal_move_count("3qk3/4P3/8/8/8/8/8/8 w - -", 4);
    }

    #[test]
    fn pawn_promotion_with_advance_or_capture() {
        assert_pseudo_legal_move_count("3q4/4P3/8/8/8/8/8/8 w - -", 8);
    }

    #[test]
    fn castle_king_side_only() {
        let pos = parse_fen("8/8/8/8/8/8/8/R3K2R w K -");

        let moves = generate_all_moves(&pos);

        assert_castling_move_count(&moves, 1);
    }

    #[test]
    fn castle_queen_side_only() {
        let pos = parse_fen("8/8/8/8/8/8/8/R3K2R w Q -");

        let moves = generate_all_moves(&pos);

        assert_castling_move_count(&moves, 1);
    }

    #[test]
    fn castle_king_and_queen_side() {
        let pos = parse_fen("8/8/8/8/8/8/8/R3K2R w KQ -");

        let moves = generate_all_moves(&pos);

        assert_castling_move_count(&moves, 2);
    }

    #[test]
    fn no_castling_when_the_target_square_is_occupied_by_a_friendly_piece() {
        let pos = parse_fen("8/8/8/8/8/8/8/R1B1K1NR w KQ -");

        let moves = generate_all_moves(&pos);

        assert_castling_move_count(&moves, 0);
    }

    #[test]
    fn no_castling_when_the_target_square_is_occupied_by_an_opponent_piece() {
        let pos = parse_fen("8/8/8/8/8/8/8/R1b1K1nR w KQ -");

        let moves = generate_all_moves(&pos);

        assert_castling_move_count(&moves, 0);
    }

    #[test]
    fn no_castling_when_a_piece_blocks_the_path() {
        let pos = parse_fen("8/8/8/8/8/8/8/RN2KB1R w KQ -");

        let moves = generate_all_moves(&pos);

        assert_castling_move_count(&moves, 0);
    }

    #[test]
    fn no_castling_when_the_king_path_is_attacked() {
        let pos = parse_fen("8/8/8/8/8/4n3/8/R3K2R w KQ -");

        let moves = generate_all_moves(&pos);

        assert_castling_move_count(&moves, 0);
    }

    #[test]
    fn no_castling_when_the_right_was_previously_lost() {
        let pos = parse_fen("8/8/8/8/8/8/8/R3K2R w Q -");

        let moves = generate_all_moves(&pos);

        assert_castling_move_count(&moves, 1);

        let castling_move = moves.iter().filter(|mv| mv.is_castling()).next().unwrap();

        assert_eq!(castling_move.from, E1);
        assert_eq!(castling_move.to, C1);
    }

    #[test]
    fn no_castling_out_of_check() {
        let pos = parse_fen("8/8/8/8/8/3n4/8/R3K2R w KQ -");

        let moves = generate_all_moves(&pos);

        assert_castling_move_count(&moves, 0);
    }

    #[test]
    fn en_passant_capture() {
        let pos = parse_fen("8/8/8/3PpP2/8/8/8/8 w - e6");

        let moves = generate_all_moves(&pos);

        assert_eq!(moves.len(), 4);
        assert_eq!(moves.iter().filter(|mv| mv.is_en_passant).count(), 2);
    }

    #[test]
    fn ignore_friendly_piece_captures() {
        assert_pseudo_legal_move_count("8/8/5p2/5P2/3N4/8/8/8 w - -", 7);
    }

    const FENS: [&str; 8] = [
        START_POS_FEN,                                                      // start position
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -", // perft position 2, white to move
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq -", // perft position 2, black to move
        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -",                            // perft position 3
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -",     // perft position 4
        "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ -",     // perft position 4, flipped
        "8/8/8/3PpP2/8/8/8/8 w - e6",                                       // en passant position
        "3q4/4P3/8/8/8/8/8/8 w - -",                                        // promotion with pawn advance or capture
    ];

    #[test]
    fn noisy_and_quiet_moves_partition_all_moves() {
        for fen in FENS {
            let pos = parse_fen(fen);
            let all_moves = generate_all_moves(&pos);
            let noisy_moves = generate_noisy_moves(&pos);
            let quiet_moves = generate_quiet_moves(&pos);

            assert!(noisy_moves.iter().all(|mv| !mv.is_quiet()));
            assert!(quiet_moves.iter().all(|mv| mv.is_quiet()));
            assert_eq!(noisy_moves.len() + quiet_moves.len(), all_moves.len());

            for mv in &all_moves {
                assert!(noisy_moves.contains(mv) || quiet_moves.contains(mv));
            }
        }
    }

    #[test]
    fn a_quiet_move_is_pseudo_legal_if_it_would_have_been_discovered_by_the_move_generator() {
        for fen in FENS {
            let pos = parse_fen(fen);
            let quiet_moves = generate_quiet_moves(&pos);

            for piece in Piece::pieces() {
                for from in 0..64 {
                    for to in 0..64 {
                        let mv = Move {
                            piece: *piece,
                            from: Square::from_index(from),
                            to: Square::from_index(to),
                            captured_piece: None,
                            promotion_piece: None,
                            is_en_passant: false,
                        };

                        assert_eq!(is_pseudo_legal_quiet_move(&pos, &mv), quiet_moves.contains(&mv));
                    }
                }
            }
        }
    }

    #[test]
    fn a_noisy_move_is_not_a_pseudo_legal_quiet_move() {
        for fen in FENS {
            let pos = parse_fen(fen);

            for mv in generate_noisy_moves(&pos) {
                assert!(!is_pseudo_legal_quiet_move(&pos, &mv));
            }
        }
    }

    fn assert_pseudo_legal_move_count(fen: &str, count: usize) {
        assert_eq!(generate_all_moves(&parse_fen(fen)).len(), count);
    }

    fn assert_legal_move_count(fen: &str, count: usize) {
        let mut pos = parse_fen(fen);
        let mut legal_move_count = 0;

        for mv in generate_all_moves(&pos) {
            pos.do_move(&mv);

            if !is_in_check(pos.opponent_colour(), &pos.board) {
                legal_move_count += 1;
            }

            pos.undo_move(&mv);
        }

        assert_eq!(legal_move_count, count);
    }

    fn assert_castling_move_count(moves: &MoveList, count: usize) {
        assert_eq!(moves.iter().filter(|mv| mv.is_castling()).count(), count);
    }
}
