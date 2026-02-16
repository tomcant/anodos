use self::{
    history::HistoryTable,
    killers::KillerMoves,
    pv::PvTable,
    report::{Report, Reporter},
    stopper::Stopper,
    tt::TranspositionTable,
};
use crate::attacks::is_in_check;
use crate::eval::*;
use crate::r#move::Move;
use crate::movegen::{MoveList, generate_all_moves};
use crate::position::Position;

pub mod report;
pub mod stopper;
pub mod time;
pub mod tt;

mod alphabeta;
mod history;
mod killers;
mod movepicker;
mod pv;
mod quiescence;
mod see;

pub const MAX_DEPTH: u8 = u8::MAX;

struct SearchState<'a> {
    pub report: Report,
    pub stopper: &'a Stopper<'a>,
    pub tt: &'a mut TranspositionTable,
    pub killers: KillerMoves,
    pub history: HistoryTable,
    pub pv: PvTable,
}

// Aspiration window tuning
const ASP_MIN_DEPTH: u8 = 4;
const ASP_BASE_DELTA: i32 = 25; // Quarter pawn
const ASP_EXPANSION_FACTOR: i32 = 2;
const ASP_MAX_RETRIES: u8 = 3;

#[rustfmt::skip]
pub fn search(
    pos: &mut Position,
    tt: &mut TranspositionTable,
    reporter: &impl Reporter,
    stopper: &Stopper,
) {
    tt.age();

    if let Some(mv) = forced_move(pos) {
        let mut report = Report::new();
        report.pv = Some((MoveList::from_slice(&[mv]), 0));
        reporter.send(&report);
        return;
    }

    let mut ss = SearchState {
        report: Report::new(),
        stopper,
        tt,
        killers: KillerMoves::new(),
        history: HistoryTable::new(),
        pv: PvTable::new(),
    };

    let mut last_eval: i32 = 0;
    let max_depth = stopper.depth.unwrap_or(MAX_DEPTH);

    for depth in 1..=max_depth {
        // Bypass aspiration search for shallow depths or near-mate situations
        let do_asp = depth >= ASP_MIN_DEPTH && last_eval.abs() < EVAL_MATE_THRESHOLD;
        let (mut delta_low, mut delta_high) = (ASP_BASE_DELTA, ASP_BASE_DELTA);

        let (mut alpha, mut beta) = if do_asp {
            (
                (last_eval - delta_low).max(EVAL_MIN),
                (last_eval + delta_high).min(EVAL_MAX),
            )
        } else {
            (EVAL_MIN, EVAL_MAX)
        };

        last_eval = {
            let mut asp_retries = 0;

            loop {
                let eval = alphabeta::search(&mut ss, pos, depth, alpha, beta, 0);

                if (eval > alpha && eval < beta) || stopper.should_stop(&ss.report) {
                    break eval;
                }

                asp_retries += 1;
                if asp_retries > ASP_MAX_RETRIES {
                    alpha = EVAL_MIN;
                    beta = EVAL_MAX;
                    continue;
                }

                if eval <= alpha {
                    delta_low *= ASP_EXPANSION_FACTOR;
                    alpha = (last_eval - delta_low).max(EVAL_MIN);
                } else {
                    delta_high *= ASP_EXPANSION_FACTOR;
                    beta = (last_eval + delta_high).min(EVAL_MAX);
                }
            }
        };

        if stopper.should_stop(&ss.report) {
            break;
        }

        ss.report.depth = depth;
        ss.report.pv = Some(sanitise_pv(pos.clone(), (ss.pv.root().clone(), last_eval)));
        ss.report.tt_usage = ss.tt.usage();

        reporter.send(&ss.report);

        if stopper.has_elapsed_soft_time_limit(&ss.report, depth) {
            break;
        }
    }
}

fn forced_move(pos: &mut Position) -> Option<Move> {
    let mut forced_move = None;
    let colour_to_move = pos.colour_to_move;

    for mv in generate_all_moves(pos) {
        pos.do_move(&mv);
        let is_illegal = is_in_check(colour_to_move, &pos.board);
        pos.undo_move(&mv);

        if is_illegal {
            continue;
        }

        if forced_move.is_some() {
            return None;
        }

        forced_move = Some(mv);
    }

    forced_move
}

fn sanitise_pv(mut pos: Position, (moves, eval): (MoveList, i32)) -> (MoveList, i32) {
    for (index, mv) in moves.iter().enumerate() {
        pos.do_move(mv);

        if pos.is_fifty_move_draw() || pos.is_repetition_draw(0) {
            return (MoveList::from_slice(&moves[..=index]), EVAL_DRAW);
        }
    }

    (moves, eval)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::piece::Piece::*;
    use crate::square::Square::*;
    use crate::testing::*;
    use std::cell::Cell;

    #[test]
    fn report_forced_moves_without_searching() {
        let mut pos = parse_fen("3R2k1/5p1p/6p1/8/8/8/8/4K3 b - - 0 1");
        let mut tt = TranspositionTable::new(1);
        let reporter = TestReporter::new();
        let mut stopper = Stopper::new();
        stopper.at_depth(Some(1));

        search(&mut pos, &mut tt, &reporter, &stopper);

        assert_eq!(reporter.nodes(), 0);
        assert_eq!(reporter.best_move(), Some(make_move(BK, G8, G7, None)));
    }

    struct TestReporter {
        nodes: Cell<u128>,
        best_move: Cell<Option<Move>>,
    }

    impl TestReporter {
        pub fn new() -> Self {
            Self {
                nodes: Cell::new(0),
                best_move: Cell::new(None),
            }
        }

        pub fn nodes(&self) -> u128 {
            self.nodes.get()
        }

        pub fn best_move(&self) -> Option<Move> {
            self.best_move.get()
        }
    }

    impl Reporter for TestReporter {
        fn send(&self, report: &Report) {
            self.nodes.set(report.nodes);

            if let Some((moves, _)) = &report.pv {
                self.best_move.set(Some(moves[0].into()));
            }
        }
    }
}
