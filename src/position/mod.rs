use crate::attacks::en_passant_attacks;
use crate::colour::Colour;
use crate::r#move::Move;
use crate::piece::Piece::{self, *};
use crate::square::{
    LIGHT_SQUARES,
    Square::{self, *},
};
use smallvec::SmallVec;

mod board;
mod castling;
mod display;
mod fen;
mod zobrist;

pub use board::Board;
pub use castling::{CastlingRight, CastlingRights};
pub use fen::START_POS_FEN;
use zobrist::ZOBRIST;

const MAX_HISTORY: usize = 256;

#[derive(Debug, Clone)]
pub struct Position {
    pub board: Board,
    pub colour_to_move: Colour,
    pub castling_rights: CastlingRights,
    pub en_passant_square: Option<Square>,
    pub half_move_clock: u8,
    pub full_move_counter: u8,
    pub key: u64,
    history: SmallVec<[HistoryEntry; MAX_HISTORY]>,
}

#[derive(Debug, Clone, Copy)]
struct HistoryEntry {
    castling_rights: CastlingRights,
    en_passant_square: Option<Square>,
    half_move_clock: u8,
    key: u64,
}

impl Position {
    pub fn new(
        board: Board,
        colour_to_move: Colour,
        castling_rights: CastlingRights,
        en_passant_square: Option<Square>,
        half_move_clock: u8,
        full_move_counter: u8,
    ) -> Self {
        let mut pos = Self {
            board,
            colour_to_move,
            castling_rights,
            en_passant_square,
            half_move_clock,
            full_move_counter,
            key: 0,
            history: SmallVec::new(),
        };
        pos.key = pos.compute_key();
        pos
    }

    pub fn startpos() -> Self {
        START_POS_FEN.parse().unwrap()
    }

    pub fn do_move(&mut self, mv: &Move) {
        let history = HistoryEntry {
            castling_rights: self.castling_rights,
            en_passant_square: self.en_passant_square,
            half_move_clock: self.half_move_clock,
            key: self.key,
        };
        self.history.push(history);

        if let Some(square) = self.en_passant_square
            && en_passant_attacks(square, self.colour_to_move, &self.board) != 0
        {
            self.key ^= ZOBRIST.en_passant_files[square.file() as usize];
        }

        self.en_passant_square = None;
        self.half_move_clock += 1;

        if let Some(capture_square) = mv.capture_square() {
            self.half_move_clock = 0;
            self.board.remove_piece(capture_square);
            self.key ^= ZOBRIST.piece_square[mv.captured_piece.unwrap()][capture_square];
        }

        if mv.piece.is_pawn() {
            self.half_move_clock = 0;

            if mv.rank_diff() == 2 {
                let square = mv.from.advance(self.colour_to_move);
                self.en_passant_square = Some(square);

                if en_passant_attacks(square, self.opponent_colour(), &self.board) != 0 {
                    self.key ^= ZOBRIST.en_passant_files[square.file() as usize];
                }
            }
        }

        if mv.piece.is_king() {
            self.castling_rights.remove_for_colour(self.colour_to_move);

            if mv.is_castling() {
                let rook = Piece::rook(self.colour_to_move);

                match mv.to {
                    C1 | C8 => {
                        let rook_to = Square::from_file_and_rank(3, mv.to.rank());
                        let rook_from = Square::from_file_and_rank(0, mv.to.rank());

                        self.board.put_piece(rook, rook_to);
                        self.board.remove_piece(rook_from);

                        self.key ^= ZOBRIST.piece_square[rook][rook_to];
                        self.key ^= ZOBRIST.piece_square[rook][rook_from];
                    }
                    G1 | G8 => {
                        let rook_to = Square::from_file_and_rank(5, mv.to.rank());
                        let rook_from = Square::from_file_and_rank(7, mv.to.rank());

                        self.board.put_piece(rook, rook_to);
                        self.board.remove_piece(rook_from);

                        self.key ^= ZOBRIST.piece_square[rook][rook_to];
                        self.key ^= ZOBRIST.piece_square[rook][rook_from];
                    }
                    _ => unreachable!(),
                };
            }
        }

        if mv.from.is_corner() {
            self.castling_rights.remove_for_square(mv.from);
        }

        if mv.to.is_corner() {
            self.castling_rights.remove_for_square(mv.to);
        }

        self.key ^= ZOBRIST.castling_rights[self.castling_rights];
        self.key ^= ZOBRIST.castling_rights[history.castling_rights];

        let to_piece = mv.promotion_piece.unwrap_or(mv.piece);
        self.board.put_piece(to_piece, mv.to);
        self.board.remove_piece(mv.from);

        self.key ^= ZOBRIST.piece_square[to_piece][mv.to];
        self.key ^= ZOBRIST.piece_square[mv.piece][mv.from];

        if self.colour_to_move == Colour::Black {
            self.full_move_counter += 1;
        }

        self.colour_to_move = self.opponent_colour();
        self.key ^= ZOBRIST.colour_to_move;

        debug_assert_eq!(self.key, self.compute_key());
    }

    pub fn undo_move(&mut self, mv: &Move) {
        let history = self.history.pop().unwrap();
        self.castling_rights = history.castling_rights;
        self.en_passant_square = history.en_passant_square;
        self.half_move_clock = history.half_move_clock;
        self.key = history.key;

        if mv.is_castling() {
            let rook = Piece::rook(self.opponent_colour());

            match mv.to {
                C1 | C8 => {
                    let rook_to = Square::from_file_and_rank(0, mv.to.rank());
                    let rook_from = Square::from_file_and_rank(3, mv.to.rank());

                    self.board.put_piece(rook, rook_to);
                    self.board.remove_piece(rook_from);
                }
                G1 | G8 => {
                    let rook_to = Square::from_file_and_rank(7, mv.to.rank());
                    let rook_from = Square::from_file_and_rank(5, mv.to.rank());

                    self.board.put_piece(rook, rook_to);
                    self.board.remove_piece(rook_from);
                }
                _ => unreachable!(),
            };
        }

        self.board.remove_piece(mv.to);
        self.board.put_piece(mv.piece, mv.from);

        if let Some(capture_square) = mv.capture_square() {
            self.board.put_piece(mv.captured_piece.unwrap(), capture_square);
        }

        self.colour_to_move = self.opponent_colour();

        if self.colour_to_move == Colour::Black {
            self.full_move_counter -= 1;
        }

        debug_assert_eq!(self.key, self.compute_key());
    }

    pub fn do_null_move(&mut self) {
        let history = HistoryEntry {
            castling_rights: self.castling_rights,
            en_passant_square: self.en_passant_square,
            half_move_clock: self.half_move_clock,
            key: self.key,
        };
        self.history.push(history);

        if let Some(square) = self.en_passant_square
            && en_passant_attacks(square, self.colour_to_move, &self.board) != 0
        {
            self.key ^= ZOBRIST.en_passant_files[square.file() as usize];
        }

        self.en_passant_square = None;
        self.half_move_clock += 1;

        if self.colour_to_move == Colour::Black {
            self.full_move_counter += 1;
        }

        self.colour_to_move = self.opponent_colour();
        self.key ^= ZOBRIST.colour_to_move;

        debug_assert_eq!(self.key, self.compute_key());
    }

    pub fn undo_null_move(&mut self) {
        let history = self.history.pop().unwrap();
        self.castling_rights = history.castling_rights;
        self.en_passant_square = history.en_passant_square;
        self.half_move_clock = history.half_move_clock;
        self.key = history.key;

        self.colour_to_move = self.opponent_colour();

        if self.colour_to_move == Colour::Black {
            self.full_move_counter -= 1;
        }

        debug_assert_eq!(self.key, self.compute_key());
    }

    pub fn is_fifty_move_draw(&self) -> bool {
        self.half_move_clock >= 100
    }

    pub fn is_repetition_draw(&self, search_ply: u8) -> bool {
        if self.half_move_clock < 8 {
            return false;
        }

        let mut counter = 0;
        let keys = self.history.iter().map(|h| h.key).rev();

        for (distance, key) in keys.enumerate().take(self.half_move_clock as usize).skip(3).step_by(2) {
            if key != self.key {
                continue;
            }

            // If the repetition occurred since the root node then we can assume that
            // neither side has anything better than a draw since both are maximising
            // their score. It's therefore safe to assume the cycle will repeat.
            if distance < search_ply as usize {
                return true;
            }

            counter += 1;
            if counter == 2 {
                return true;
            }
        }

        false
    }

    pub fn has_checkmate_material(&self) -> bool {
        let board = &self.board;

        if board.count(WP) != 0
            || board.count(BP) != 0
            || board.count(WR) != 0
            || board.count(BR) != 0
            || board.count(WQ) != 0
            || board.count(BQ) != 0
        {
            return true;
        }

        let wb = board.count(WB);
        let bb = board.count(BB);
        let wn = board.count(WN);
        let bn = board.count(BN);

        // KNB, KNNN
        if (wb != 0 && wn != 0) || (bb != 0 && bn != 0) || wn >= 3 || bn >= 3 {
            return true;
        }

        // KBB, bishop pair
        (wb == 2 && (board.pieces(WB) & LIGHT_SQUARES).count_ones() == 1)
            || (bb == 2 && (board.pieces(BB) & LIGHT_SQUARES).count_ones() == 1)
    }

    pub fn opponent_colour(&self) -> Colour {
        self.colour_to_move.flip()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::*;

    #[test]
    fn move_a_piece() {
        let mut pos = parse_fen("8/8/8/8/8/8/8/5R2 w - -");

        let mv = Move {
            piece: WR,
            from: F1,
            to: F4,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };

        pos.do_move(&mv);

        assert_eq!(pos.board.piece_at(mv.to), Some(WR));
        assert!(!pos.board.has_piece_at(mv.from));
        assert_eq!(pos.colour_to_move, Colour::Black);
    }

    #[test]
    fn undo_moving_a_piece() {
        let mut pos = parse_fen("4k3/8/8/8/8/8/8/4KR2 w - -");

        let mv = Move {
            piece: WR,
            from: F1,
            to: F4,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };
        pos.do_move(&mv);

        pos.undo_move(&mv);

        assert_eq!(pos.board.piece_at(mv.from), Some(WR));
        assert!(!pos.board.has_piece_at(mv.to));
        assert_eq!(pos.colour_to_move, Colour::White);
    }

    #[test]
    fn capture_a_piece() {
        let mut pos = parse_fen("8/8/8/5p2/3N4/8/8/8 w - -");

        let mv = Move {
            piece: WN,
            from: D4,
            to: F5,
            captured_piece: Some(BP),
            promotion_piece: None,
            is_en_passant: false,
        };

        pos.do_move(&mv);

        assert_eq!(pos.board.piece_at(mv.to), Some(WN));
        assert!(!pos.board.has_piece_at(mv.from));
    }

    #[test]
    fn undo_capturing_a_piece() {
        let mut pos = parse_fen("4k3/8/8/5p2/3N4/8/8/4K3 w - -");

        let mv = Move {
            piece: WN,
            from: D4,
            to: F5,
            captured_piece: Some(BP),
            promotion_piece: None,
            is_en_passant: false,
        };
        pos.do_move(&mv);

        pos.undo_move(&mv);

        assert_eq!(pos.board.piece_at(mv.from), Some(WN));
        assert_eq!(pos.board.piece_at(mv.to), Some(BP));
    }

    #[test]
    fn castle_king_side() {
        let mut pos = parse_fen("8/8/8/8/8/8/8/4K2R w K -");

        let mv = Move {
            piece: WK,
            from: E1,
            to: G1,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };

        pos.do_move(&mv);

        assert_eq!(pos.castling_rights, CastlingRights::none());

        assert_eq!(pos.board.piece_at(mv.to), Some(WK));
        assert_eq!(pos.board.piece_at(F1), Some(WR));

        assert!(!pos.board.has_piece_at(mv.from));
        assert!(!pos.board.has_piece_at(H1));
    }

    #[test]
    fn undo_castle_king_side() {
        let mut pos = parse_fen("4k3/8/8/8/8/8/8/4K2R w K -");

        let mv = Move {
            piece: WK,
            from: E1,
            to: G1,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };
        pos.do_move(&mv);

        pos.undo_move(&mv);

        assert_eq!(pos.castling_rights, CastlingRights::from(&[CastlingRight::WhiteKing]));

        assert_eq!(pos.board.piece_at(mv.from), Some(WK));
        assert_eq!(pos.board.piece_at(H1), Some(WR));

        assert!(!pos.board.has_piece_at(mv.to));
        assert!(!pos.board.has_piece_at(F1));
    }

    #[test]
    fn moving_a_rook_removes_the_relevant_castling_rights() {
        let mut pos = parse_fen("8/8/8/8/8/8/8/R3K2R w KQ -");

        let mv = Move {
            piece: WR,
            from: H1,
            to: G1,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };

        pos.do_move(&mv);

        assert_eq!(pos.castling_rights, CastlingRights::from(&[CastlingRight::WhiteQueen]));
    }

    #[test]
    fn capturing_a_rook_removes_the_relevant_castling_rights() {
        let mut pos = parse_fen("8/8/8/8/3b4/8/8/R3K2R b KQ -");

        let mv = Move {
            piece: BB,
            from: D4,
            to: A1,
            captured_piece: Some(WR),
            promotion_piece: None,
            is_en_passant: false,
        };

        pos.do_move(&mv);

        assert_eq!(pos.castling_rights, CastlingRights::from(&[CastlingRight::WhiteKing]));
    }

    #[test]
    fn promote_a_pawn() {
        let mut pos = parse_fen("8/4P3/8/8/8/8/8/8 w - -");

        let mv = Move {
            piece: WP,
            from: E7,
            to: E8,
            captured_piece: None,
            promotion_piece: Some(WN),
            is_en_passant: false,
        };

        pos.do_move(&mv);

        assert_eq!(pos.board.piece_at(mv.to), mv.promotion_piece);
        assert!(!pos.board.has_piece_at(mv.from));
    }

    #[test]
    fn undo_promoting_a_pawn() {
        let mut pos = parse_fen("4k3/2P5/8/8/8/8/8/4K3 w - -");

        let mv = Move {
            piece: WP,
            from: C7,
            to: C8,
            captured_piece: None,
            promotion_piece: Some(WN),
            is_en_passant: false,
        };
        pos.do_move(&mv);

        pos.undo_move(&mv);

        assert_eq!(pos.board.piece_at(mv.from), Some(WP));
        assert!(!pos.board.has_piece_at(mv.to));
    }

    #[test]
    fn undo_promoting_a_pawn_with_capture() {
        let mut pos = parse_fen("1n2k3/2P5/8/8/8/8/8/4K3 w - -");

        let mv = Move {
            piece: WP,
            from: C7,
            to: B8,
            captured_piece: Some(BN),
            promotion_piece: Some(WN),
            is_en_passant: false,
        };
        pos.do_move(&mv);

        pos.undo_move(&mv);

        assert_eq!(pos.board.piece_at(mv.from), Some(WP));
        assert_eq!(pos.board.piece_at(mv.to), mv.captured_piece);
    }

    #[test]
    fn capture_a_pawn_en_passant() {
        let mut pos = parse_fen("8/8/8/3Pp3/8/8/8/8 w - e6");

        let mv = Move {
            piece: WP,
            from: D5,
            to: E6,
            captured_piece: Some(BP),
            promotion_piece: None,
            is_en_passant: true,
        };

        pos.do_move(&mv);

        assert_eq!(pos.board.piece_at(mv.to), Some(WP));
        assert!(!pos.board.has_piece_at(E5));
        assert!(!pos.board.has_piece_at(mv.from));
    }

    #[test]
    fn undo_capturing_a_pawn_en_passant() {
        let mut pos = parse_fen("4k3/8/8/3Pp3/8/8/8/4K3 w - e6");

        let mv = Move {
            piece: WP,
            from: D5,
            to: E6,
            captured_piece: Some(BP),
            promotion_piece: None,
            is_en_passant: true,
        };
        pos.do_move(&mv);

        pos.undo_move(&mv);

        assert_eq!(pos.en_passant_square, Some(mv.to));
        assert_eq!(pos.board.piece_at(mv.from), Some(WP));
        assert_eq!(pos.board.piece_at(E5), Some(BP));
        assert!(!pos.board.has_piece_at(mv.to));
    }

    #[test]
    fn set_the_en_passant_square_for_a_white_double_pawn_advance() {
        let mut pos = parse_fen("8/8/8/8/8/8/4P3/8 w - -");

        let mv = Move {
            piece: WP,
            from: E2,
            to: E4,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };

        pos.do_move(&mv);

        assert_eq!(pos.en_passant_square, Some(E3));
    }

    #[test]
    fn set_the_en_passant_square_for_a_black_double_pawn_advance() {
        let mut pos = parse_fen("8/4p3/8/8/8/8/8/8 b - -");

        let mv = Move {
            piece: BP,
            from: E7,
            to: E5,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };

        pos.do_move(&mv);

        assert_eq!(pos.en_passant_square, Some(E6));
    }

    #[test]
    fn reset_the_en_passant_square_when_undoing_a_double_pawn_advance() {
        let mut pos = Position::startpos();

        let mv = Move {
            piece: WP,
            from: E2,
            to: E4,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };
        pos.do_move(&mv);

        pos.undo_move(&mv);

        assert_eq!(pos.en_passant_square, None);
    }

    #[test]
    fn restore_the_previous_en_passant_square_when_undoing_a_move() {
        let mut pos = Position::startpos();

        let mv = Move {
            piece: WP,
            from: E2,
            to: E4,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };
        pos.do_move(&mv);

        let mv = Move {
            piece: BN,
            from: G8,
            to: F6,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };
        pos.do_move(&mv);

        pos.undo_move(&mv);

        assert_eq!(pos.en_passant_square, Some(E3));
    }

    #[test]
    fn increment_the_half_move_clock_for_non_pawn_or_non_capture_moves() {
        let mut pos = parse_fen("8/4p3/8/8/8/8/4P3/4K3 w - -");

        let mv = Move {
            piece: WK,
            from: E1,
            to: F2,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };

        pos.do_move(&mv);

        assert_eq!(pos.half_move_clock, 1);
    }

    #[test]
    fn reset_the_half_move_clock_when_a_pawn_moves() {
        let mut pos = parse_fen("8/4p3/8/8/8/8/4P3/8 w - - 1 1");

        let mv = Move {
            piece: WP,
            from: E2,
            to: E4,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };

        pos.do_move(&mv);

        assert_eq!(pos.half_move_clock, 0);
    }

    #[test]
    fn reset_the_half_move_clock_when_a_piece_is_captured() {
        let mut pos = parse_fen("8/4p3/8/8/8/8/4Q3/8 w - - 1 1");

        let mv = Move {
            piece: WQ,
            from: E2,
            to: E7,
            captured_piece: Some(BP),
            promotion_piece: None,
            is_en_passant: false,
        };

        pos.do_move(&mv);

        assert_eq!(pos.half_move_clock, 0);
    }

    #[test]
    fn increment_the_full_move_counter_when_black_moves() {
        let mut pos = Position::startpos();

        assert_eq!(pos.full_move_counter, 1);

        let white_move = Move {
            piece: WP,
            from: E2,
            to: E4,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };
        pos.do_move(&white_move);

        assert_eq!(pos.full_move_counter, 1);

        let black_move = Move {
            piece: BP,
            from: E7,
            to: E5,
            captured_piece: None,
            promotion_piece: None,
            is_en_passant: false,
        };
        pos.do_move(&black_move);

        assert_eq!(pos.full_move_counter, 2);

        pos.undo_move(&black_move);

        assert_eq!(pos.full_move_counter, 1);

        pos.undo_move(&white_move);

        assert_eq!(pos.full_move_counter, 1);
    }

    #[test]
    fn detect_repetition_draw_from_start_position() {
        let mut pos = Position::startpos();

        let mut moves = [
            make_move(WN, G1, F3, None), // Nf3
            make_move(BN, G8, F6, None), // Nf6
            make_move(WN, F3, G1, None), // Ng1
            make_move(BN, F6, G8, None), // Ng8, first repetition
            make_move(WN, G1, F3, None), // Nf3
            make_move(BN, G8, F6, None), // Nf6
            make_move(WN, F3, G1, None), // Ng1
            make_move(BN, F6, G8, None), // Ng8, second repetition, threefold
        ];

        for (index, mv) in moves.iter_mut().enumerate() {
            pos.do_move(&mv);

            let expect_repetition_draw = index == 7;
            assert_eq!(
                pos.is_repetition_draw(0),
                expect_repetition_draw,
                "Position should {} a repetition draw at ply {}",
                if expect_repetition_draw { "be" } else { "not be" },
                index + 1
            );
        }
    }

    #[test]
    fn detect_repetition_draw_from_middle_game_position() {
        let mut pos = parse_fen("1r1q1rk1/2p2pp1/2Q4p/pB2P3/P2P4/b6P/2R2PP1/3R2K1 b - - 10 33");

        let mut moves = [
            make_move(BR, B8, C8, None), // Rc8
            make_move(WB, B5, A6, None), // Ba6
            make_move(BR, C8, B8, None), // Rb8
            make_move(WB, A6, B5, None), // Bb5, first repetition
            make_move(BR, B8, C8, None), // Rc8
            make_move(WB, B5, A6, None), // Ba6
            make_move(BR, C8, B8, None), // Rb8
            make_move(WB, A6, B5, None), // Bb5, second repetition, threefold
        ];

        for (index, mv) in moves.iter_mut().enumerate() {
            pos.do_move(&mv);

            let expect_repetition_draw = index == 7;
            assert_eq!(
                pos.is_repetition_draw(0),
                expect_repetition_draw,
                "Position should {} a repetition draw at ply {}",
                if expect_repetition_draw { "be" } else { "not be" },
                index + 1
            );
        }
    }

    #[test]
    fn repetition_draw_not_counted_when_castling_rights_differ() {
        let mut pos = Position::startpos();

        let mut moves = [
            make_move(WN, G1, F3, None), // Nf3, the pieces revisit these squares later
            make_move(BN, G8, F6, None), // Nf6
            make_move(WR, H1, G1, None), // Rg1, removes white king-side castling rights
            make_move(BN, F6, G8, None), // Ng8
            make_move(WR, G1, H1, None), // Rh1, first repetition of piece placement
            make_move(BN, G8, F6, None), // Nf6
            make_move(WN, F3, G1, None), // Ng1
            make_move(BN, F6, G8, None), // Ng8
            make_move(WN, G1, F3, None), // Nf3, second repetition but doesn't count due to castling rights
        ];

        for (index, mv) in moves.iter_mut().enumerate() {
            pos.do_move(&mv);

            assert!(
                !pos.is_repetition_draw(0),
                "Position should not be considered a repetition draw at ply {}",
                index + 1
            );
        }
    }

    #[test]
    fn repetition_draw_not_counted_when_en_passant_availability_differs() {
        let mut pos = parse_fen("4k3/4p3/8/3P4/8/8/8/4K1Nn w - -");

        let mut moves = [
            make_move(WN, G1, F3, None), // Nf3
            make_move(BP, E7, E5, None), // e5, the pieces revisit these squares later
            make_move(WN, F3, G1, None), // Ng1, en passant availability expires
            make_move(BN, H1, G3, None), // Ng3
            make_move(WN, G1, F3, None), // Nf3
            make_move(BN, G3, H1, None), // Nh1, first repetition of piece placement
            make_move(WN, F3, G1, None), // Ng1
            make_move(BN, H1, G3, None), // Ng3
            make_move(WN, G1, F3, None), // Nf3
            make_move(BN, G3, H1, None), // Nh1, second repetition but doesn't count due to en passant availability
        ];

        for (index, mv) in moves.iter_mut().enumerate() {
            pos.do_move(&mv);

            assert!(
                !pos.is_repetition_draw(0),
                "Position should not be considered a repetition draw at ply {}",
                index + 1
            );
        }
    }

    #[test]
    fn has_checkmate_material() {
        let cases = [
            "4k3/8/8/8/8/8/4P3/4K3 w - -",    // KPvK, white
            "4k3/4p3/8/8/8/8/8/4K3 w - -",    // KPvK, black
            "4k3/8/8/8/8/8/8/1NB1K3 w - -",   // KNBvK, white
            "1nb1k3/8/8/8/8/8/8/4K3 w - -",   // KNBvK, black
            "4k3/8/8/8/8/8/8/2B1KB2 w - -",   // KBBvK, white, bishop pair
            "2b1kb2/8/8/8/8/8/8/4K3 w - -",   // KBBvK, black, bishop pair
            "4k3/8/8/8/8/8/8/1NN1K1N1 w - -", // KNNNvK, white
            "1nn1k1n1/8/8/8/8/8/8/4K3 w - -", // KNNNvK, black
            "4k3/8/8/8/8/8/8/R3K3 w - -",     // KRvK, white
            "r3k3/8/8/8/8/8/8/4K3 w - -",     // KRvK, black
            "4k3/8/8/8/8/8/8/3QK3 w - -",     // KQvK, white
            "3qk3/8/8/8/8/8/8/4K3 w - -",     // KQvK, black
        ];
        for fen in cases {
            let pos = parse_fen(fen);

            assert!(
                pos.has_checkmate_material(),
                "Position '{fen}' does have checkmate material"
            );
        }
    }

    #[test]
    fn not_has_checkmate_material() {
        let cases = [
            "4k3/8/8/8/8/8/8/4K3 w - -",      // KvK
            "4k3/8/8/8/8/8/8/4K1N1 w - -",    // KNvK
            "4k3/8/8/8/8/8/8/2B1K3 w - -",    // KBvK
            "4k3/8/8/8/8/8/8/1N2K1N1 w - -",  // KNNvK
            "4k1n1/8/8/8/8/8/8/4K1N1 w - -",  // KNvKN
            "4k1b1/8/8/8/8/8/8/4K1N1 w - -",  // KNvKB
            "4kb2/8/8/8/8/8/8/2B1K3 w - -",   // KBvKB, same colour bishops
            "4k3/8/8/8/8/8/8/B1B1K1B1 w - -", // KBBBvK
        ];
        for fen in cases {
            let pos = parse_fen(fen);

            assert!(
                !pos.has_checkmate_material(),
                "Position '{fen}' does not have checkmate material",
            );
        }
    }
}
