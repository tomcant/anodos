use super::{
    movepicker::{MovePicker, MovePickerMode},
    *,
};
use crate::attacks::is_in_check;
use crate::eval::terms::PIECE_WEIGHTS;

const DELTA_MARGIN: i32 = 200;

pub fn search(ss: &mut SearchState, pos: &mut Position, mut alpha: i32, beta: i32) -> i32 {
    ss.report.nodes += 1;

    let eval = eval(pos);

    if eval >= beta {
        return beta;
    }

    if eval > alpha {
        alpha = eval;
    }

    let colour_to_move = pos.colour_to_move;
    let mut move_picker = MovePicker::new(MovePickerMode::Noisy);

    while let Some(mv) = move_picker.pick(pos, ss) {
        // Delta pruning: if the static eval plus the captured piece value is
        // still less than alpha then prune this move because it is hopeless.
        let mut delta = mv.captured_piece.map_or(0, |piece| PIECE_WEIGHTS[piece]);

        if let Some(promo) = mv.promotion_piece {
            delta += PIECE_WEIGHTS[promo] - PIECE_WEIGHTS[mv.piece];
        }

        if eval + delta + DELTA_MARGIN < alpha {
            continue;
        }

        pos.do_move(&mv);

        if is_in_check(colour_to_move, &pos.board) {
            pos.undo_move(&mv);
            continue;
        }

        let eval = -search(ss, pos, -beta, -alpha);

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
