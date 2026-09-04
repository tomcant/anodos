use super::{SearchState, history::HISTORY_SCORE_MAX, see};
use crate::eval::terms::PIECE_WEIGHTS;
use crate::r#move::Move;
use crate::movegen::{MAX_MOVES, generate_noisy_moves, generate_quiet_moves, is_pseudo_legal_quiet_move};
use crate::piece::Piece;
use crate::position::Position;
use smallvec::SmallVec;

const MVV_LVA_SCORE_MAX: i32 = 90_000; // KxQ = Queen (900) * 100 - King (0)

// The "good/bad capture" and "quiet" scores are lower bounds, others are exact.
const SCORE_GOOD_CAPTURE: i32 = 0;
const SCORE_PROMOTION: i32 = MVV_LVA_SCORE_MAX + 1;
const SCORE_QUIET: i32 = SCORE_PROMOTION + 1;
const SCORE_BAD_CAPTURE: i32 = SCORE_QUIET + 2 * HISTORY_SCORE_MAX + 1;

pub enum MovePickerMode {
    AllMoves { tt_move: Option<Move>, ply: u8 },
    Noisy,
}

impl MovePickerMode {
    fn tt_move(&self) -> Option<Move> {
        match self {
            Self::AllMoves { tt_move, .. } => *tt_move,
            Self::Noisy => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MovePickerStage {
    TtMove,
    GenerateNoisy,
    Noisy,
    Killer { index: usize },
    GenerateQuiet,
    Quiet,
}

pub struct MovePicker {
    mode: MovePickerMode,
    stage: MovePickerStage,
    killers: [Option<Move>; 2],
    moves: SmallVec<[(Move, i32); MAX_MOVES]>,
    current_index: usize,
}

impl MovePicker {
    pub fn new(mode: MovePickerMode) -> Self {
        Self {
            mode,
            stage: MovePickerStage::TtMove,
            killers: [None; 2],
            moves: SmallVec::new(),
            current_index: 0,
        }
    }

    pub fn pick(&mut self, pos: &Position, ss: &SearchState) -> Option<Move> {
        loop {
            match self.stage {
                MovePickerStage::TtMove => {
                    self.stage = MovePickerStage::GenerateNoisy;

                    if let Some(tt_move) = self.mode.tt_move() {
                        return Some(tt_move);
                    }
                }
                MovePickerStage::GenerateNoisy => {
                    self.stage = MovePickerStage::Noisy;
                    self.generate_noisy(pos);
                }
                MovePickerStage::Noisy => {
                    let Some(index) = self.find_best_index(SCORE_BAD_CAPTURE) else {
                        self.stage = match self.mode {
                            MovePickerMode::AllMoves { ply, .. } => {
                                self.killers = [ss.killers.probe(ply, 0), ss.killers.probe(ply, 1)];
                                MovePickerStage::Killer { index: 0 }
                            }
                            MovePickerMode::Noisy => return None,
                        };
                        continue;
                    };

                    let (mv, _) = self.moves[index];

                    // Defer bad captures until after quiets.
                    if mv.captured_piece.is_some() && !see::see_ge(&pos.board, &mv) {
                        self.moves[index].1 += SCORE_BAD_CAPTURE;
                        continue;
                    }

                    self.take(index);
                    return Some(mv);
                }
                MovePickerStage::Killer { index } => {
                    if index == self.killers.len() {
                        self.stage = MovePickerStage::GenerateQuiet;
                        continue;
                    }

                    self.stage = MovePickerStage::Killer { index: index + 1 };

                    if let Some(killer) = self.playable_killer(pos, index) {
                        return Some(killer);
                    }
                }
                MovePickerStage::GenerateQuiet => {
                    self.stage = MovePickerStage::Quiet;
                    self.generate_quiet(pos, ss);
                }
                MovePickerStage::Quiet => {
                    let index = self.find_best_index(i32::MAX)?;
                    return Some(self.take(index));
                }
            }
        }
    }

    #[inline(never)]
    fn generate_noisy(&mut self, pos: &Position) {
        for mv in generate_noisy_moves(pos) {
            if self.is_tt_move(&mv) {
                continue;
            }

            let score = match mv.captured_piece {
                Some(victim) => SCORE_GOOD_CAPTURE + mvv_lva(victim, mv.piece),
                None => SCORE_PROMOTION,
            };

            self.moves.push((mv, score));
        }
    }

    #[inline(never)]
    fn generate_quiet(&mut self, pos: &Position, ss: &SearchState) {
        for mv in generate_quiet_moves(pos) {
            if self.is_tt_move(&mv) || self.is_killer(&mv) {
                continue;
            }

            let score = SCORE_QUIET + HISTORY_SCORE_MAX - ss.history.probe(mv.piece, mv.to);

            self.moves.push((mv, score));
        }
    }

    fn playable_killer(&self, pos: &Position, index: usize) -> Option<Move> {
        self.killers[index].filter(|killer| !self.is_tt_move(killer) && is_pseudo_legal_quiet_move(pos, killer))
    }

    fn is_killer(&self, mv: &Move) -> bool {
        self.killers
            .iter()
            .any(|killer| killer.is_some_and(|killer| mv.equals(&killer)))
    }

    fn is_tt_move(&self, mv: &Move) -> bool {
        self.mode.tt_move().is_some_and(|tt_move| mv.equals(&tt_move))
    }

    fn find_best_index(&self, max_score: i32) -> Option<usize> {
        (self.current_index..self.moves.len())
            .min_by_key(|&index| self.moves[index].1)
            .filter(|&index| self.moves[index].1 < max_score)
    }

    fn take(&mut self, index: usize) -> Move {
        self.moves.swap(self.current_index, index);
        let (mv, _) = self.moves[self.current_index];
        self.current_index += 1;
        mv
    }
}

#[inline]
fn mvv_lva(victim: Piece, attacker: Piece) -> i32 {
    MVV_LVA_SCORE_MAX - PIECE_WEIGHTS[victim] * 100 + PIECE_WEIGHTS[attacker]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::colour::Colour;
    use crate::movegen::generate_all_moves;
    use crate::piece::Piece::*;
    use crate::position::START_POS_FEN;
    use crate::search::{stopper::Stopper, tt::TranspositionTable};
    use crate::square::Square::*;
    use crate::testing::*;

    #[test]
    fn mvv_lva_scores_stay_within_the_good_capture_range() {
        for victim in Piece::pieces().iter().filter(|piece| !piece.is_king()) {
            for attacker in Piece::pieces() {
                let score = mvv_lva(*victim, *attacker);
                assert!((SCORE_GOOD_CAPTURE..SCORE_PROMOTION).contains(&score));
            }
        }
    }

    #[test]
    fn order_moves_by_good_captures_mvv_lva_then_promotions_then_killers_then_history_then_bad_captures() {
        let quiet1 = make_move(WP, G2, G4, None);
        let quiet2 = make_move(WP, G2, G3, None);
        let quiet3 = make_move(WP, C4, C5, None);
        let killer1 = make_move(WP, A2, A3, None);
        let killer2 = make_move(WP, B2, B3, None);
        let pawn_x_pawn = make_move(WP, C4, B5, Some(BP));
        let pawn_x_queen = make_move(WP, C4, D5, Some(BQ));
        let knight_x_bishop = make_move(WN, F4, D3, Some(BB));
        let knight_x_queen = make_move(WN, F4, D5, Some(BQ));
        let knight_x_rook = make_move(WN, F4, G6, Some(BR));
        let knight_x_knight = make_move(WN, F4, H3, Some(BN));
        let knight_x_pawn = make_move(WN, F4, E6, Some(BP)); // Bad capture
        let promotion = make_promotion_move(Colour::White, A7, A8, WQ);

        let killer_ply = 0;
        let mut tt = TranspositionTable::new(1);
        let stopper = Stopper::new();
        let mut ss = SearchState::new(&mut tt, &stopper);
        ss.killers.store(killer_ply, &killer2);
        ss.killers.store(killer_ply, &killer1);
        ss.history.store(100, WP, G4); // Quiet 1 is good, score high
        ss.history.store(-100, WP, C5); // Quiet 3 is bad, score low

        let pos = parse_fen("7k/P7/4p1r1/1p1q4/2P2N2/3b3n/PP4P1/4K3 w - -");

        let mut picker = MovePicker::new(MovePickerMode::AllMoves {
            tt_move: None,
            ply: killer_ply,
        });

        let picked = pick_all(&mut picker, &pos, &ss);
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
    fn order_noisy_moves_by_good_captures_mvv_lva_then_promotions() {
        let pawn_x_pawn = make_move(WP, C4, B5, Some(BP));
        let pawn_x_queen = make_move(WP, C4, D5, Some(BQ));
        let knight_x_bishop = make_move(WN, F4, D3, Some(BB));
        let knight_x_queen = make_move(WN, F4, D5, Some(BQ));
        let knight_x_rook = make_move(WN, F4, G6, Some(BR));
        let knight_x_knight = make_move(WN, F4, H3, Some(BN));
        let knight_x_pawn = make_move(WN, F4, E6, Some(BP)); // Bad capture
        let promotion = make_promotion_move(Colour::White, A7, A8, WQ);

        let mut tt = TranspositionTable::new(1);
        let stopper = Stopper::new();
        let ss = SearchState::new(&mut tt, &stopper);

        let pos = parse_fen("7k/P7/4p1r1/1p1q4/2P2N2/3b3n/8/4K3 w - -");
        let mut picker = MovePicker::new(MovePickerMode::Noisy);

        let picked = pick_all(&mut picker, &pos, &ss);
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

    #[test]
    fn pick_the_transposition_table_move_first_and_only_once() {
        let quiet = make_move(WP, G2, G4, None);
        let good_capture = make_move(WN, F4, D5, Some(BQ));
        let bad_capture = make_move(WN, F4, E6, Some(BP));

        let killer_ply = 0;
        let mut tt = TranspositionTable::new(1);
        let stopper = Stopper::new();
        let mut ss = SearchState::new(&mut tt, &stopper);
        ss.killers.store(killer_ply, &quiet); // A killer that is also the TT move is only tried once

        let pos = parse_fen("7k/P7/4p1r1/1p1q4/2P2N2/3b3n/PP4P1/4K3 w - -");
        let all_moves = generate_all_moves(&pos);

        for tt_move in [quiet, good_capture, bad_capture] {
            let mut picker = MovePicker::new(MovePickerMode::AllMoves {
                tt_move: Some(tt_move),
                ply: killer_ply,
            });

            let picked = pick_all(&mut picker, &pos, &ss);

            assert_eq!(picked.first(), Some(&tt_move));
            assert_eq!(picked.iter().filter(|mv| mv.equals(&tt_move)).count(), 1);
            assert_eq!(picked.len(), all_moves.len());
        }
    }

    #[test]
    fn skip_killers_that_are_not_pseudo_legal() {
        let playable_killer = make_move(WP, G2, G3, None);
        let missing_piece_killer = make_move(WN, B1, C3, None);

        let killer_ply = 0;
        let mut tt = TranspositionTable::new(1);
        let stopper = Stopper::new();
        let mut ss = SearchState::new(&mut tt, &stopper);
        ss.killers.store(killer_ply, &playable_killer);
        ss.killers.store(killer_ply, &missing_piece_killer); // Stored last, so tried first

        let pos = parse_fen("7k/P7/4p1r1/1p1q4/2P2N2/3b3n/PP4P1/4K3 w - -");
        let all_moves = generate_all_moves(&pos);

        let mut picker = MovePicker::new(MovePickerMode::AllMoves {
            tt_move: None,
            ply: killer_ply,
        });

        let picked = pick_all(&mut picker, &pos, &ss);

        assert_eq!(picked.len(), all_moves.len());

        for mv in &all_moves {
            assert!(picked.contains(mv));
        }

        assert!(!picked.contains(&missing_piece_killer));

        let index_killer = picked.iter().position(|mv| *mv == playable_killer).unwrap();
        let index_first_quiet = picked.iter().position(|mv| mv.is_quiet()).unwrap();

        assert_eq!(index_killer, index_first_quiet);
    }

    #[test]
    fn pick_every_move_exactly_once() {
        let fens = [
            START_POS_FEN,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -",
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -",
            "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ -",
            "8/8/8/3PpP2/8/8/8/8 w - e6",
        ];

        let mut tt = TranspositionTable::new(1);
        let stopper = Stopper::new();
        let ss = SearchState::new(&mut tt, &stopper);

        for fen in fens {
            let pos = parse_fen(fen);
            let all_moves = generate_all_moves(&pos);
            let mut picker = MovePicker::new(MovePickerMode::AllMoves { tt_move: None, ply: 0 });

            let picked = pick_all(&mut picker, &pos, &ss);

            assert_eq!(picked.len(), all_moves.len());

            for mv in &all_moves {
                assert!(picked.contains(mv));
            }
        }
    }

    fn pick_all(picker: &mut MovePicker, pos: &Position, ss: &SearchState) -> Vec<Move> {
        std::iter::from_fn(|| picker.pick(pos, ss)).collect()
    }
}
