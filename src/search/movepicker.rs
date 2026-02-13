use super::{
    history::{HISTORY_SCORE_MAX, HistoryTable},
    killers::KillerMoves,
    see,
};
use crate::eval::terms::PIECE_WEIGHTS;
use crate::movegen::{MAX_MOVES, Move, generate_all_moves, generate_non_quiet_moves};
use crate::piece::Piece;
use crate::position::Position;
use smallvec::SmallVec;

const MVV_LVA_SCORE_MAX: i32 = 89_900; // PxQ = Queen (900) * 100 - Pawn (100)

// The "good/bad capture" and "quiet" scores are lower bounds, others are exact.
const SCORE_GOOD_CAPTURE: i32 = 0;
const SCORE_PROMOTION: i32 = MVV_LVA_SCORE_MAX + 1;
const SCORE_KILLER_1: i32 = SCORE_PROMOTION + 1;
const SCORE_KILLER_2: i32 = SCORE_KILLER_1 + 1;
const SCORE_QUIET: i32 = SCORE_KILLER_2 + 1;
const SCORE_BAD_CAPTURE: i32 = SCORE_QUIET + HISTORY_SCORE_MAX + 1;

pub enum MovePickerMode<'a> {
    AllMoves {
        killers: &'a KillerMoves,
        history: &'a HistoryTable,
        ply: u8,
    },
    NonQuiets,
}

pub struct MovePicker {
    moves: SmallVec<[(Move, i32); MAX_MOVES]>,
    current_index: usize,
}

impl MovePicker {
    pub fn new(pos: &Position, mode: MovePickerMode<'_>) -> Self {
        let mut scored_moves = SmallVec::new();

        let mvv_lva = |victim: Piece, attacker: Piece| -> i32 {
            MVV_LVA_SCORE_MAX - PIECE_WEIGHTS[victim] * 100 + PIECE_WEIGHTS[attacker]
        };

        match &mode {
            MovePickerMode::AllMoves { killers, history, ply } => {
                let killer1 = killers.probe(*ply, 0);
                let killer2 = killers.probe(*ply, 1);

                let score = |mv: &Move| {
                    if let Some(victim) = mv.captured_piece {
                        return if see::see_ge(&pos.board, mv) {
                            SCORE_GOOD_CAPTURE + mvv_lva(victim, mv.piece)
                        } else {
                            SCORE_BAD_CAPTURE + mvv_lva(victim, mv.piece)
                        };
                    }

                    if mv.promotion_piece.is_some() {
                        return SCORE_PROMOTION;
                    }

                    if let Some(killer) = killer1
                        && mv.equals(&killer)
                    {
                        return SCORE_KILLER_1;
                    }

                    if let Some(killer) = killer2
                        && mv.equals(&killer)
                    {
                        return SCORE_KILLER_2;
                    }

                    SCORE_QUIET + HISTORY_SCORE_MAX - history.probe(mv.piece, mv.to)
                };
                for mv in generate_all_moves(pos) {
                    scored_moves.push((mv, score(&mv)));
                }
            }
            MovePickerMode::NonQuiets => {
                for mv in generate_non_quiet_moves(pos) {
                    let score = if let Some(victim) = mv.captured_piece {
                        if see::see_ge(&pos.board, &mv) {
                            SCORE_GOOD_CAPTURE + mvv_lva(victim, mv.piece)
                        } else {
                            continue; // Skip bad captures in quiescence search
                        }
                    } else {
                        SCORE_PROMOTION
                    };
                    scored_moves.push((mv, score));
                }
            }
        };

        Self {
            moves: scored_moves,
            current_index: 0,
        }
    }

    pub fn pick(&mut self) -> Option<Move> {
        if self.current_index >= self.moves.len() {
            return None;
        }

        let next_index = (self.current_index..self.moves.len())
            .min_by_key(|&i| self.moves[i].1)
            .unwrap();

        let (next_move, _) = self.moves[next_index];

        self.moves.swap(self.current_index, next_index);
        self.current_index += 1;

        Some(next_move)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::colour::Colour;
    use crate::piece::Piece;
    use crate::square::Square;
    use crate::testing::*;

    #[test]
    fn order_moves_by_good_captures_mvv_lva_then_promotions_then_killers_then_history_then_bad_captures() {
        let quiet1 = make_move(Piece::WP, Square::G2, Square::G4, None);
        let quiet2 = make_move(Piece::WP, Square::G2, Square::G3, None);
        let quiet3 = make_move(Piece::WP, Square::C4, Square::C5, None);
        let killer1 = make_move(Piece::WP, Square::A2, Square::A3, None);
        let killer2 = make_move(Piece::WP, Square::B2, Square::B3, None);
        let pawn_x_pawn = make_move(Piece::WP, Square::C4, Square::B5, Some(Piece::BP));
        let pawn_x_queen = make_move(Piece::WP, Square::C4, Square::D5, Some(Piece::BQ));
        let knight_x_bishop = make_move(Piece::WN, Square::F4, Square::D3, Some(Piece::BB));
        let knight_x_queen = make_move(Piece::WN, Square::F4, Square::D5, Some(Piece::BQ));
        let knight_x_rook = make_move(Piece::WN, Square::F4, Square::G6, Some(Piece::BR));
        let knight_x_knight = make_move(Piece::WN, Square::F4, Square::H3, Some(Piece::BN));
        let knight_x_pawn = make_move(Piece::WN, Square::F4, Square::E6, Some(Piece::BP)); // Bad capture
        let promotion = make_promotion_move(Colour::White, Square::A7, Square::A8, Piece::WQ);

        let killer_ply = 0;
        let mut killers = KillerMoves::new();
        killers.store(killer_ply, &killer2);
        killers.store(killer_ply, &killer1);

        let mut history = HistoryTable::new();
        history.store(100, Piece::WP, Square::G4); // Quiet 1 is good, score high
        history.store(-100, Piece::WP, Square::C5); // Quiet 3 is bad, score low

        let mut picker = MovePicker::new(
            &parse_fen("7k/P7/4p1r1/1p1q4/2P2N2/3b3n/PP4P1/4K3 w - - 0 1"),
            MovePickerMode::AllMoves {
                killers: &killers,
                history: &history,
                ply: killer_ply,
            },
        );

        let picked = std::iter::from_fn(|| picker.pick()).collect::<Vec<Move>>();

        let index = |target: &Move| picked.iter().position(|mv| mv == target).unwrap();

        let index_pawn_x_queen = index(&pawn_x_queen);
        let index_knight_x_queen = index(&knight_x_queen);
        let index_knight_x_rook = index(&knight_x_rook);
        let index_knight_x_bishop = index(&knight_x_bishop);
        let index_knight_x_knight = index(&knight_x_knight);
        let index_knight_x_pawn = index(&knight_x_pawn); // Bad capture
        let index_pawn_x_pawn = index(&pawn_x_pawn);
        let index_promotion = index(&promotion);
        let index_killer1 = index(&killer1);
        let index_killer2 = index(&killer2);
        let index_quiet1 = index(&quiet1);
        let index_quiet2 = index(&quiet2);
        let index_quiet3 = index(&quiet3);

        // Good captures ordered by MVV/LVA.
        assert!(index_pawn_x_queen < index_knight_x_queen);
        assert!(index_knight_x_queen < index_knight_x_rook);
        assert!(index_knight_x_rook < index_knight_x_bishop);
        assert!(index_knight_x_bishop < index_knight_x_knight);
        assert!(index_knight_x_knight < index_pawn_x_pawn);

        // Then promotions, then killers, then remaining quiets ordered by history.
        assert!(index_pawn_x_pawn < index_promotion);
        assert!(index_promotion < index_killer1);
        assert!(index_killer1 < index_killer2);
        assert!(index_killer2 < index_quiet1);
        assert!(index_quiet1 < index_quiet2);
        assert!(index_quiet2 < index_quiet3);

        // Bad captures last.
        assert!(index_quiet3 < index_knight_x_pawn);
    }

    #[test]
    fn non_quiet_order_moves_by_good_captures_mvv_lva() {
        let pawn_x_pawn = make_move(Piece::WP, Square::C4, Square::B5, Some(Piece::BP));
        let pawn_x_queen = make_move(Piece::WP, Square::C4, Square::D5, Some(Piece::BQ));
        let knight_x_bishop = make_move(Piece::WN, Square::F4, Square::D3, Some(Piece::BB));
        let knight_x_queen = make_move(Piece::WN, Square::F4, Square::D5, Some(Piece::BQ));
        let knight_x_rook = make_move(Piece::WN, Square::F4, Square::G6, Some(Piece::BR));
        let knight_x_knight = make_move(Piece::WN, Square::F4, Square::H3, Some(Piece::BN));
        let knight_x_pawn = make_move(Piece::WN, Square::F4, Square::E6, Some(Piece::BP)); // Bad capture
        let promotion = make_promotion_move(Colour::White, Square::A7, Square::A8, Piece::WQ);

        let mut picker = MovePicker::new(
            &parse_fen("7k/P7/4p1r1/1p1q4/2P2N2/3b3n/8/4K3 w - - 0 1"),
            MovePickerMode::NonQuiets,
        );

        let picked = std::iter::from_fn(|| picker.pick()).collect::<Vec<Move>>();

        let index = |target: &Move| picked.iter().position(|mv| mv == target).unwrap();

        let index_pawn_x_queen = index(&pawn_x_queen);
        let index_knight_x_queen = index(&knight_x_queen);
        let index_knight_x_rook = index(&knight_x_rook);
        let index_knight_x_bishop = index(&knight_x_bishop);
        let index_knight_x_knight = index(&knight_x_knight);
        let index_pawn_x_pawn = index(&pawn_x_pawn);
        let index_promotion = index(&promotion);

        assert!(index_pawn_x_queen < index_knight_x_queen);
        assert!(index_knight_x_queen < index_knight_x_rook);
        assert!(index_knight_x_rook < index_knight_x_bishop);
        assert!(index_knight_x_bishop < index_knight_x_knight);
        assert!(index_knight_x_knight < index_pawn_x_pawn);
        assert!(index_pawn_x_pawn < index_promotion);

        // Bad captures are pruned for quiescence search.
        assert!(!picked.contains(&knight_x_pawn));
    }
}
