use self::command::{UciCommand::*, handle};
use crate::colour::Colour;
use crate::eval::EVAL_MATE_THRESHOLD;
use crate::position::Position;
use crate::search::{
    stopper::Stopper,
    time::TimeLimit,
    tt::{self, TranspositionTable},
};
use std::{
    io,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
        mpsc,
    },
    thread,
};

pub mod command;
pub mod r#move;

mod reporter;

pub fn main() {
    let (uci_tx, uci_rx) = mpsc::channel();
    let stop_signal = Arc::new(AtomicBool::new(false));
    let pos = Arc::new(Mutex::new(Position::startpos()));
    let tt = Arc::new(Mutex::new(TranspositionTable::new(tt::DEFAULT_SIZE_MB)));

    thread::spawn(move || {
        loop {
            let mut buffer = String::new();
            io::stdin().read_line(&mut buffer).unwrap();
            let command = buffer.trim();

            if command.is_empty() {
                continue;
            }

            match command.parse() {
                Ok(parsed) => uci_tx.send(parsed).unwrap(),
                Err(err) => println!("error: {err}"),
            }
        }
    });

    loop {
        match uci_rx.recv().unwrap() {
            Init => handle::init(),
            IsReady => handle::is_ready(),
            NewGame => handle::new_game(&mut pos.lock().unwrap(), &mut tt.lock().unwrap()),
            PrintBoard => handle::print_board(&pos.lock().unwrap()),
            PrintFen => handle::print_fen(&pos.lock().unwrap()),
            Perft(depth) => handle::perft(depth, &pos.lock().unwrap()),
            DoMove(mv) => handle::do_move(mv, &mut pos.lock().unwrap()),
            Position(fen, moves) => handle::position(fen, moves, &mut pos.lock().unwrap()),
            Go(params) => {
                let stop_signal = Arc::clone(&stop_signal);
                let pos = Arc::clone(&pos);
                let tt = Arc::clone(&tt);

                thread::spawn(move || {
                    stop_signal.store(false, Ordering::Relaxed);

                    let mut stopper = Stopper::new();
                    stopper.at_signal(&stop_signal);
                    stopper.at_depth(params.depth);
                    stopper.at_mate(params.mate);
                    stopper.at_nodes(params.nodes);

                    let time = params.movetime.map(TimeLimit::fixed).or_else(|| {
                        let pos = pos.lock().unwrap();

                        let (time_left, time_inc) = match pos.colour_to_move {
                            Colour::White => (params.wtime, params.winc),
                            _ => (params.btime, params.binc),
                        };

                        if time_left.is_some() {
                            // Don't waste time searching beyond mate if there's a time limit.
                            stopper.at_eval(Some(EVAL_MATE_THRESHOLD));
                        }

                        time_left.map(|t| TimeLimit::dynamic(t, time_inc))
                    });
                    stopper.at_time(time);

                    // Clone the position so that searching doesn't block
                    // this thread and we can still handle other commands.
                    let mut pos = pos.lock().unwrap().clone();
                    handle::go(&mut pos, &mut tt.lock().unwrap(), &stopper);
                });
            }
            SetOption(name, value) => handle::set_option(name, value, &mut tt.lock().unwrap()),
            Stop => stop_signal.store(true, Ordering::Relaxed),
            Quit => break,
        }
    }
}
