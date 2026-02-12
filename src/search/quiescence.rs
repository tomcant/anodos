use super::{
    movepicker::{MovePicker, MovePickerMode},
    *,
};
use crate::attacks::is_in_check;

pub fn search(pos: &mut Position, mut alpha: i32, beta: i32, report: &mut Report) -> i32 {
    report.nodes += 1;

    let eval = eval(pos);

    if eval >= beta {
        return beta;
    }

    if eval > alpha {
        alpha = eval;
    }

    let colour_to_move = pos.colour_to_move;
    let mut move_picker = MovePicker::new(pos, MovePickerMode::NonQuiets);

    while let Some(mv) = move_picker.pick() {
        pos.do_move(&mv);

        if is_in_check(colour_to_move, &pos.board) {
            pos.undo_move(&mv);
            continue;
        }

        let eval = -search(pos, -beta, -alpha, report);

        pos.undo_move(&mv);

        if eval >= beta {
            return beta;
        }

        if eval > alpha {
            alpha = eval;
        }
    }

    alpha
}
