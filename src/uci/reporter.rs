use super::r#move::UciMove;
use crate::search::report::{Report, Reporter};
use std::cell::Cell;

pub struct UciReporter {
    best_move: Cell<Option<UciMove>>,
}

impl UciReporter {
    pub fn new() -> Self {
        Self {
            best_move: Cell::new(None),
        }
    }

    pub fn best_move(&self) -> Option<UciMove> {
        self.best_move.get()
    }
}

impl Reporter for UciReporter {
    fn send(&self, report: &Report) {
        let mut info = vec![
            format!("depth {}", report.depth),
            format!("nodes {}", report.nodes),
            format!("nps {}", report.nodes * 1000 / report.elapsed().as_millis().max(1)),
            format!("hashfull {}", report.tt_usage),
            format!("time {}", report.elapsed().as_millis()),
        ];

        if let Some((moves, eval)) = &report.pv {
            if let Some(moves_until_mate) = report.moves_until_mate() {
                info.push(format!("score mate {}", moves_until_mate as i32 * eval.signum()));
            } else {
                info.push(format!("score cp {}", eval));
            }

            if !moves.is_empty() {
                info.push(format!(
                    "pv {}",
                    moves
                        .iter()
                        .map(|mv| format!("{}", UciMove::from(*mv)))
                        .collect::<Vec<String>>()
                        .join(" ")
                ));

                self.best_move.set(Some(moves[0].into()));
            }
        }

        println!("info {}", info.join(" "));
    }
}
