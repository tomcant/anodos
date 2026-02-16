use crate::attacks::is_in_check;
use crate::movegen::generate_all_moves;
use crate::position::Position;
use crate::uci::r#move::UciMove;

pub fn perft(pos: &mut Position, depth: u8) -> u128 {
    if depth == 0 {
        return 1;
    }

    let mut nodes = 0;

    for mv in generate_all_moves(pos) {
        pos.do_move(&mv);

        if !is_in_check(pos.opponent_colour(), &pos.board) {
            nodes += perft(pos, depth - 1);
        }

        pos.undo_move(&mv);
    }

    nodes
}

pub fn divide(pos: &mut Position, depth: u8) -> u128 {
    if depth == 0 {
        return 1;
    }

    let mut nodes = 0;

    for mv in generate_all_moves(pos) {
        pos.do_move(&mv);

        if !is_in_check(pos.opponent_colour(), &pos.board) {
            let count = perft(pos, depth - 1);
            println!("{}: {}", UciMove::from(mv), count);
            nodes += count;
        }

        pos.undo_move(&mv);
    }

    nodes
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::position::START_POS_FEN;
    use crate::testing::*;

    #[test]
    fn perft_start_position_shallow() {
        assert_perft(START_POS_FEN, 4, 197_281);
    }

    #[test]
    #[ignore]
    fn perft_start_position() {
        assert_perft(START_POS_FEN, 6, 119_060_324);
    }

    #[test]
    #[ignore]
    fn perft_position_2() {
        assert_perft(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            5,
            193_690_690,
        );
    }

    #[test]
    #[ignore]
    fn perft_position_3() {
        assert_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 7, 178_633_661);
    }

    #[test]
    #[ignore]
    fn perft_position_4() {
        assert_perft(
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
            6,
            706_045_033,
        );
    }

    #[test]
    #[ignore]
    fn perft_position_4_flipped() {
        assert_perft(
            "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1",
            6,
            706_045_033,
        );
    }

    #[test]
    #[ignore]
    fn perft_position_5() {
        assert_perft(
            "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
            5,
            89_941_194,
        );
    }

    #[test]
    #[ignore]
    fn perft_position_6() {
        assert_perft(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
            5,
            164_075_551,
        );
    }

    fn assert_perft(fen: &str, depth: u8, expected_move_count: u128) {
        assert_eq!(perft(&mut parse_fen(fen), depth), expected_move_count);
    }
}
