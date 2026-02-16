use super::terms::EvalTerm;
use crate::piece::Piece::*;
use crate::position::Board;

const MAX_PHASE: i32 = 24;

pub fn phase_eval(eval: EvalTerm, board: &Board) -> i32 {
    let phase = phase(board);

    (eval.mg() * phase + eval.eg() * (MAX_PHASE - phase)) / MAX_PHASE
}

fn phase(board: &Board) -> i32 {
    let knights = board.count(WN) + board.count(BN);
    let bishops = board.count(WB) + board.count(BB);
    let rooks = board.count(WR) + board.count(BR);
    let queens = board.count(WQ) + board.count(BQ);

    MAX_PHASE.min((knights + bishops + 2 * rooks + 4 * queens) as i32)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::position::Position;
    use crate::testing::*;

    #[test]
    fn startpos_has_max_phase() {
        let pos = Position::startpos();
        assert_eq!(phase(&pos.board), MAX_PHASE);
    }

    #[test]
    fn kings_only_has_zero_phase() {
        let pos = parse_fen("4k3/8/8/8/8/8/8/4K3 w - - 0 1");
        assert_eq!(phase(&pos.board), 0);
    }
}
