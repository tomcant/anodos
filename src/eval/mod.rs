use crate::colour::Colour;
use crate::position::Position;
use crate::search::MAX_DEPTH;

mod phase;

pub mod terms;

use phase::phase_eval;
use terms::{EvalTerm, TERMS};

pub const EVAL_MAX: i32 = 10_000;
pub const EVAL_MIN: i32 = -EVAL_MAX;
pub const EVAL_DRAW: i32 = 0;
pub const EVAL_MATE: i32 = EVAL_MAX;
pub const EVAL_MATE_THRESHOLD: i32 = EVAL_MATE - MAX_DEPTH as i32;

pub fn eval(pos: &Position) -> i32 {
    if !pos.has_checkmate_material() {
        return EVAL_DRAW;
    }

    let eval = TERMS.iter().fold(EvalTerm::zero(), |acc, term| {
        acc + term(Colour::White, &pos.board) - term(Colour::Black, &pos.board)
    });

    let phased_eval = phase_eval(eval, &pos.board);

    match pos.colour_to_move {
        Colour::White => phased_eval,
        _ => -phased_eval,
    }
}
