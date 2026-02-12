use super::{
    history::HISTORY_SCORE_MAX,
    movepicker::{MovePicker, MovePickerMode},
    tt::Bound,
    *,
};
use crate::attacks::is_in_check;
use crate::colour::Colour;
use crate::piece::Piece;
use crate::position::Board;
use smallvec::SmallVec;

const LMR_HISTORY_THRESHOLD: i32 = HISTORY_SCORE_MAX / 4;
const LMP_THRESHOLDS: [u8; 5] = [0, 5, 9, 14, 21];

#[rustfmt::skip]
pub fn search(
    ss: &mut SearchState,
    pos: &mut Position,
    mut depth: u8,
    mut alpha: i32,
    beta: i32,
    ply: u8,
) -> i32 {
    ss.pv.clear(ply);

    if ss.stopper.should_stop(&ss.report) {
        return 0;
    }

    if pos.is_fifty_move_draw() || pos.is_repetition_draw(ply) {
        return EVAL_DRAW;
    }

    if depth == 0 {
        if !is_in_check(pos.colour_to_move, &pos.board) {
            return quiescence::search(pos, alpha, beta, &mut ss.report);
        }

        // Extend the search if we're in check so that quiescence doesn't need
        // to consider possible evasions and can remain focused on captures.
        depth = 1;
    }

    let is_pv_node = beta - alpha > 1;
    let mut tt_move = None;

    if let Some(entry) = ss.tt.probe(pos.key) {
        // Don't cut off at PV nodes since we need to build the full PV.
        if !is_pv_node && entry.depth >= depth {
            let eval = tt::eval_out(entry.eval, ply);

            match entry.bound {
                Bound::Exact => return eval,
                Bound::Lower if eval >= beta => return beta,
                Bound::Upper if eval <= alpha => return alpha,
                _ => (),
            };
        }

        tt_move = entry.mv;
    }

    ss.report.nodes += 1;

    let colour_to_move = pos.colour_to_move;
    let in_check = is_in_check(colour_to_move, &pos.board);

    // Static eval used for futility pruning heuristics at non-PV nodes. This is
    // intentionally restricted to a PVS null-window so we don't prune PV nodes
    // where we need accurate scores.
    let futility_base_eval = if !is_pv_node
        && !in_check
        && ply > 0
        && depth <= 5
        && alpha > -EVAL_MATE_THRESHOLD
        && beta < EVAL_MATE_THRESHOLD
    {
        let eval = eval(pos);

        // Reverse futility pruning: if the static eval is already well above
        // beta at shallow depths, assume this node will fail high.
        let safe_to_prune = match tt_move {
            Some(mv) => mv.captured_piece.is_none(),
            None => true,
        };
        if safe_to_prune && eval - depth as i32 * 100 >= beta {
            ss.tt.store(pos.key, depth, tt::eval_in(beta, ply), Bound::Lower, tt_move);
            return beta;
        }

        if depth <= 3 { Some(eval) } else { None }
    } else {
        None
    };

    // Null-move pruning: if not in check and with sufficient depth/material, try
    // a null move to quickly detect beta cutoffs.
    if depth >= 3 && !in_check && has_non_pawn_material(&pos.board, colour_to_move) {
        pos.do_null_move();

        let reduction = if depth > 6 { 3 } else { 2 };
        let eval = -search(ss, pos, depth - reduction - 1, -beta, -beta + 1, ply + 1);

        pos.undo_null_move();

        if eval >= beta {
            ss.tt.store(pos.key, depth, tt::eval_in(eval, ply), Bound::Lower, None);
            return beta;
        }
    }

    let mut tt_bound = Bound::Upper;
    let mut searched_quiets: SmallVec<[_; 32]> = SmallVec::new();
    let mut has_searched_one = false;
    let mut move_number = 0;

    // Search the TT move before generating other moves because there's a good
    // chance it leads to a cutoff
    if let Some(mv) = tt_move {
        pos.do_move(&mv);

        let eval = -search(ss, pos, depth - 1, -beta, -alpha, ply + 1);

        pos.undo_move(&mv);

        if eval >= beta {
            if mv.is_quiet() {
                ss.killers.store(ply, &mv);

                let history_bonus = depth as i32 * depth as i32;
                ss.history.store(history_bonus, mv.piece, mv.to);
            }

            ss.tt.store(pos.key, depth, tt::eval_in(eval, ply), Bound::Lower, tt_move);
            return beta;
        }

        if eval > alpha {
            alpha = eval;
            tt_bound = Bound::Exact;
            ss.pv.update(ply, mv);
        }

        if mv.is_quiet() {
            searched_quiets.push((mv.piece, mv.to));
        }

        has_searched_one = true;
        move_number = 1;
    }

    let mut move_picker = MovePicker::new(
        pos,
        MovePickerMode::AllMoves {
            killers: &ss.killers,
            history: &ss.history,
            ply,
        },
    );

    while let Some(mv) = move_picker.pick() {
        if tt_move.is_some() && mv.equals(&tt_move.unwrap()) {
            continue;
        }

        pos.do_move(&mv);

        if is_in_check(colour_to_move, &pos.board) {
            pos.undo_move(&mv);
            continue;
        }

        move_number += 1;

        let gives_check = is_in_check(pos.colour_to_move, &pos.board);

        // Late move pruning: skip searching quiet moves late in the move list
        // at lower depths as they're less likely to affect the outcome due to
        // (hopefully) stronger prior moves.
        if ply > 0
            && depth <= 4
            && !in_check
            && !gives_check
            && mv.is_quiet()
            && move_number >= LMP_THRESHOLDS[depth as usize]
        {
            pos.undo_move(&mv);
            continue;
        }

        // Futility pruning: if the static eval plus a margin is not enough to
        // improve alpha and the move is a quiet non-promotion then prune this
        // move. This helps skip hopeless quiet moves near leaf nodes.
        if !gives_check
            && mv.is_quiet()
            && let Some(eval) = futility_base_eval
            && eval + depth as i32 * 100 <= alpha
        {
            pos.undo_move(&mv);
            continue;
        }

        // Principal variation search: after one move has been searched with the
        // full window (assumed best due to ordering), try the remaining moves
        // with a narrow window. This is cheaper because it's not searching for
        // the exact eval, just to show that it fails high or low. Only in the
        // rare case the eval lies between alpha and beta do we pay the cost of
        // a full window re-search to obtain the exact eval and PV.
        let mut eval;

        if has_searched_one {
            // Late Move Reductions: for moves that are quiet, non-checking, and
            // played later in the move order, we search them at reduced depth
            // because they're less likely to raise alpha.
            let reduction = if !is_pv_node
                && depth >= 3
                && move_number >= 4
                && !in_check
                && !gives_check
                && mv.is_quiet()
                && !ss.killers.is_killer(ply, &mv)
                && ss.history.probe(mv.piece, mv.to) < LMR_HISTORY_THRESHOLD
            {
                (log2(depth) * log2(move_number) / 3).min(depth.saturating_sub(2))
            } else {
                0
            };

            eval = -search(ss, pos, depth - reduction - 1, -alpha - 1, -alpha, ply + 1);

            // If the reduced search raised alpha then re-search at full depth
            // to see if the move is actually good.
            if eval > alpha && reduction > 0 {
                eval = -search(ss, pos, depth - 1, -alpha - 1, -alpha, ply + 1);
            }

            // If the zero-window PVS raised alpha then re-search at full window
            // to obtain the exact eval and PV.
            if eval > alpha && eval < beta {
                eval = -search(ss, pos, depth - 1, -beta, -alpha, ply + 1);
            }
        } else {
            eval = -search(ss, pos, depth - 1, -beta, -alpha, ply + 1);
        }

        pos.undo_move(&mv);

        if eval >= beta {
            if mv.is_quiet() {
                ss.killers.store(ply, &mv);

                let history_bonus = depth as i32 * depth as i32;
                ss.history.store(history_bonus, mv.piece, mv.to);

                for &(piece, to) in &searched_quiets {
                    ss.history.store(-history_bonus, piece, to);
                }
            }

            ss.tt.store(pos.key, depth, tt::eval_in(eval, ply), Bound::Lower, Some(mv));
            return beta;
        }

        if mv.is_quiet() {
            searched_quiets.push((mv.piece, mv.to));
        }

        if eval > alpha {
            alpha = eval;
            tt_bound = Bound::Exact;
            tt_move = Some(mv);
            ss.pv.update(ply, mv);
        }

        has_searched_one = true;
    }

    if move_number == 0 {
        return if in_check { -EVAL_MATE + ply as i32 } else { EVAL_DRAW };
    }

    ss.tt.store(pos.key, depth, tt::eval_in(alpha, ply), tt_bound, tt_move);

    alpha
}

#[inline]
fn has_non_pawn_material(board: &Board, colour: Colour) -> bool {
    let knights = board.count_pieces(Piece::knight(colour));
    let bishops = board.count_pieces(Piece::bishop(colour));
    let rooks = board.count_pieces(Piece::rook(colour));
    let queens = board.count_pieces(Piece::queen(colour));

    (knights + bishops + rooks + queens) > 0
}

#[inline]
fn log2(n: u8) -> u8 {
    debug_assert!(n > 0);
    7 - n.leading_zeros() as u8
}
