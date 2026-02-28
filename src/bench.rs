use crate::position::START_POS_FEN;
use crate::search::{
    report::{Report, Reporter},
    search,
    stopper::Stopper,
    tt::{self, TranspositionTable},
};
use std::cell::Cell;
use std::io::{self, IsTerminal, Write};
use std::time::Instant;

const BENCH_DEFAULT_DEPTH: u8 = 13;
const BENCH_DEFAULT_TT_MB: usize = tt::DEFAULT_SIZE_MB;

// The start position and 49 positions chosen at random from the "Win at Chess" suite.
// https://www.chessprogramming.org/Win_at_Chess
const BENCH_FENS: &[&str] = &[
    START_POS_FEN,
    "2b2rk1/p1p4p/2p1p1p1/br2N1Q1/1p2q3/8/PB3PPP/3R1RK1 w - -",
    "1r3rk1/5pb1/p2p2p1/Q1n1q2p/1NP1P3/3p1P1B/PP1R3P/1K2R3 b - -",
    "1Qq5/2P1p1kp/3r1pp1/8/8/7P/p4PP1/2R3K1 b - -",
    "5rk1/p1q3pp/1p1r4/2p1pp1Q/1PPn1P2/3B3P/P2R2P1/3R2K1 b - -",
    "1k6/ppp4p/1n2pq2/1N2Rb2/2P2Q2/8/P4KPP/3r1B2 b - -",
    "r6k/p1Q4p/2p1b1rq/4p3/B3P3/4P3/PPP3P1/4RRK1 b - -",
    "2k1rb1r/ppp3pp/2np1q2/5b2/2B2P2/2P1BQ2/PP1N1P1P/2KR3R b - -",
    "7Q/ppp2q2/3p2k1/P2Ppr1N/1PP5/7R/5rP1/6K1 b - -",
    "2b5/1r6/2kBp1p1/p2pP1P1/2pP4/1pP3K1/1R3P2/8 b - -",
    "6k1/5ppp/1q6/2b5/8/2R1pPP1/1P2Q2P/7K w - -",
    "3r1rk1/pp1q1ppp/3pn3/2pN4/5PP1/P5PQ/1PP1B3/1K1R4 w - -",
    "r1bqrk2/pp1n1n1p/3p1p2/P1pP1P1Q/2PpP1NP/6R1/2PB4/4RBK1 w - -",
    "3r1r1k/1b4pp/ppn1p3/4Pp1R/Pn5P/3P4/4QP2/1qB1NKR1 w - -",
    "r5k1/pp1RR1pp/1b6/6r1/2p5/B6P/P4qPK/3Q4 w - -",
    "2kr1r2/p6p/5Pp1/2p5/1qp2Q1P/7R/PP6/1KR5 w - -",
    "2k4r/1pr1n3/p1p1q2p/5pp1/3P1P2/P1P1P3/1R2Q1PP/1RB3K1 w - -",
    "3r1rk1/1pb1qp1p/2p3p1/p7/P2Np2R/1P5P/1BP2PP1/3Q1BK1 w - -",
    "r3r1k1/pp1n1ppp/2p5/4Pb2/2B2P2/B1P5/P5PP/R2R2K1 w - -",
    "3Q4/p3b1k1/2p2rPp/2q5/4B3/P2P4/7P/6RK w - -",
    "1r3b1k/p4rpp/4pp2/3q4/2ppbPPQ/6RK/PP5P/2B1NR2 b - -",
    "8/8/8/1p5r/p1p1k1pN/P2pBpP1/1P1K1P2/8 b - -",
    "r4rk1/p1B1bpp1/1p2pn1p/8/2PP4/3B1P2/qP2QP1P/3R1RK1 w - -",
    "5r1k/pp4pp/2p5/2b1P3/4Pq2/1PB1p3/P3Q1PP/3N2K1 b - -",
    "2r5/1r6/4pNpk/3pP1qp/8/2P1QP2/5PK1/R7 w - -",
    "2r2b1r/p1Nk2pp/3p1p2/N2Qn3/4P3/q6P/P4PP1/1R3K1R w - -",
    "8/7p/5k2/5p2/p1p2P2/Pr1pPK2/1P1R3P/8 b - -",
    "6rk/1pp2Qrp/3p1B2/1pb1p2R/3n1q2/3P4/PPP3PP/R6K w - -",
    "r5r1/pQ5p/1qp2R2/2k1p3/4P3/2PP4/P1P3PP/6K1 w - -",
    "r2q3k/p2P3p/1p3p2/3QP1r1/8/B7/P5PP/2R3K1 w - -",
    "2R5/2R4p/5p1k/6n1/8/1P2QPPq/r7/6K1 w - -",
    "2rq1bk1/p4p1p/1p4p1/3b4/3B1Q2/8/P4PpP/3RR1K1 w - -",
    "r1b1qN1k/1pp3p1/p2p3n/4p1B1/8/1BP4Q/PP3KPP/8 w - -",
    "r3q1kr/ppp5/3p2pQ/8/3PP1b1/5R2/PPP3P1/5RK1 w - -",
    "4r3/1Q1qk2p/p4pp1/3Pb3/P7/6PP/5P2/4R1K1 w - -",
    "6k1/6p1/2p4p/4Pp2/4b1qP/2Br4/1P2RQPK/8 b - -",
    "1k6/5RP1/1P6/1K6/6r1/8/8/8 w - -",
    "r2r2k1/1R2qp2/p5pp/2P5/b1PN1b2/P7/1Q3PPP/1B1R2K1 b - -",
    "2b2r1k/4q2p/3p2pQ/2pBp3/8/6P1/1PP2P1P/R5K1 w - -",
    "8/3b2kp/4p1p1/pr1n4/N1N4P/1P4P1/1K3P2/3R4 w - -",
    "1r2k1r1/5p2/b3p3/1p2b1B1/3p3P/3B4/PP2KP2/2R3R1 w - -",
    "r1b1qrk1/2p2ppp/pb1pnn2/1p2pNB1/3PP3/1BP5/PP2QPPP/RN1R2K1 w - -",
    "rn3k1r/pp2bBpp/2p2n2/q5N1/3P4/1P6/P1P3PP/R1BQ1RK1 w - -",
    "6k1/5p2/p5np/4B3/3P4/1PP1q3/P3r1QP/6RK w - -",
    "5rk1/1ppb3p/p1pb4/6q1/3P1p1r/2P1R2P/PP1BQ1P1/5RKN w - -",
    "r1q3rk/1ppbb1p1/4Np1p/p3pP2/P3P3/2N4R/1PP1Q1PP/3R2K1 w - -",
    "2r1k3/6pr/p1nBP3/1p3p1p/2q5/2P5/P1R4P/K2Q2R1 w - -",
    "R7/P4k2/8/8/8/8/r7/6K1 w - -",
    "6r1/3Pn1qk/p1p1P1rp/2Q2p2/2P5/1P4P1/P3R2P/5RK1 b - -",
    "5r1k/p5pp/8/1P1pq3/P1p2nR1/Q7/5BPP/6K1 b - -",
];

pub fn run(cli_args: &[String]) {
    let mut depth = BENCH_DEFAULT_DEPTH;
    let mut tt_mb = BENCH_DEFAULT_TT_MB;
    let mut iter = cli_args.iter();

    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--depth" => depth = iter.next().unwrap().parse().unwrap(),
            "--tt-mb" => tt_mb = iter.next().unwrap().parse().unwrap(),
            _ => {}
        }
    }

    println!("\nRunning benchmark with:");
    println!("- Depth: {depth}");
    println!("- TT size: {tt_mb} MB\n");

    let stdout = io::stdout();
    let mut out = stdout.lock();
    let is_tty = stdout.is_terminal();
    let (c1, c2) = if is_tty { ("\x1b[90m", "\x1b[0m") } else { ("", "") };

    let mut tt = TranspositionTable::new(tt_mb);
    let reporter = BenchReporter::new();
    let mut stopper = Stopper::new();
    stopper.at_depth(Some(depth));

    let mut total_nodes = 0;
    let total_fens = BENCH_FENS.len();
    let max_fen_len = BENCH_FENS.iter().map(|fen| fen.len()).max().unwrap();
    let bench_started_at = Instant::now();

    for (i, fen) in BENCH_FENS.iter().enumerate() {
        let mut pos = fen.parse().unwrap();
        let search_started_at = Instant::now();

        let running_line = format!(
            "{:02}/{} {c1}{fen:width$}{c2} time: {c1}{:<10}{c2} nodes: {c1}{:<10}{c2} nps: {c1}{:<10}{c2}",
            i + 1,
            total_fens,
            "--",
            "--",
            "--",
            width = max_fen_len + 2
        );
        if is_tty {
            write!(out, "{running_line}").unwrap();
            out.flush().unwrap();
        } else {
            writeln!(out, "{running_line}").unwrap();
        }

        search(&mut pos, &mut tt, &reporter, &stopper);

        let elapsed = search_started_at.elapsed();
        let nodes = reporter.nodes();
        let nps = nodes * 1000 / elapsed.as_millis().max(1);

        let result_line = format!(
            "{:02}/{} {c1}{fen:width$}{c2} time: {c1}{elapsed:10.2?}{c2} nodes: {c1}{nodes:<10}{c2} nps: {c1}{nps:<10}{c2}",
            i + 1,
            total_fens,
            width = max_fen_len + 2,
        );
        if is_tty {
            write!(out, "\r\x1b[2K{result_line}\n").unwrap();
        } else {
            writeln!(out, "{result_line}").unwrap();
        }

        total_nodes += nodes;

        tt.clear();
    }

    let elapsed = bench_started_at.elapsed();

    writeln!(out, "\ntime: {c1}{:10.2?}{c2}", elapsed).unwrap();
    writeln!(out, "nodes: {c1}{total_nodes}{c2}").unwrap();
    writeln!(out, "nps: {c1}{}{c2}", total_nodes * 1000 / elapsed.as_millis().max(1)).unwrap();
}

struct BenchReporter {
    nodes: Cell<u128>,
}

impl BenchReporter {
    pub fn new() -> Self {
        Self { nodes: Cell::new(0) }
    }

    pub fn nodes(&self) -> u128 {
        self.nodes.get()
    }
}

impl Reporter for BenchReporter {
    fn send(&self, report: &Report) {
        self.nodes.set(report.nodes);
    }
}
