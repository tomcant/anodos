use super::report::Report;
use super::time::TimeLimit;
use crate::eval::EVAL_MIN;
use crate::r#move::Move;
use std::cell::Cell;
use std::sync::atomic::{AtomicBool, Ordering};

const STOPPER_NODES_MASK: u128 = 255;
const SOFT_STOP_MIN_DEPTH: u8 = 5;
const EVAL_MULTIPLIER_MIN: f32 = 0.5;
const EVAL_MULTIPLIER_MAX: f32 = 1.5;
const EVAL_MULTIPLIER_SCALE: f32 = 200.0;
const BEST_MOVE_MULTIPLIERS: [f32; 5] = [2.0, 1.2, 0.9, 0.8, 0.75];

pub struct Stopper<'a> {
    pub depth: Option<u8>,
    time: Option<TimeLimit>,
    eval: Option<i32>,
    nodes: Option<u128>,
    signal: Option<&'a AtomicBool>,
    stability: Cell<SearchStability>,
}

impl<'a> Stopper<'a> {
    pub fn new() -> Self {
        Self {
            depth: None,
            time: None,
            eval: None,
            nodes: None,
            signal: None,
            stability: Cell::new(SearchStability::default()),
        }
    }

    pub fn at_depth(&mut self, depth: Option<u8>) {
        self.depth = depth;
    }

    pub fn at_time(&mut self, time: Option<TimeLimit>) {
        self.time = time;
    }

    pub fn at_eval(&mut self, eval: Option<i32>) {
        self.eval = eval;
    }

    pub fn at_nodes(&mut self, nodes: Option<u128>) {
        self.nodes = nodes;
    }

    pub fn at_signal(&mut self, signal: &'a AtomicBool) {
        self.signal = Some(signal);
    }

    pub fn should_stop(&self, report: &Report) -> bool {
        if report.nodes & STOPPER_NODES_MASK != 0 {
            return false;
        }

        if let Some(signal) = self.signal
            && signal.load(Ordering::Relaxed)
        {
            return true;
        }

        if let Some(time) = &self.time
            && report.elapsed() > time.hard()
        {
            return true;
        }

        if let Some(eval) = self.eval
            && report.eval().unwrap_or(0).abs() >= eval
        {
            return true;
        }

        if let Some(nodes) = self.nodes
            && report.nodes > nodes
        {
            return true;
        }

        false
    }

    pub fn has_elapsed_soft_time_limit(&self, report: &Report, depth: u8) -> bool {
        let Some(TimeLimit::Dynamic { soft, hard }) = &self.time else {
            return false;
        };

        let best_move = report.best_move();
        let eval = report.eval().unwrap_or(0);
        let mut stability = self.stability.get();

        let has_elapsed = depth >= SOFT_STOP_MIN_DEPTH && {
            if best_move == stability.last_best_move {
                stability.best_move_stability += 1;
            } else {
                stability.best_move_stability = 0;
            }

            let best_move_stability = stability.best_move_stability.min(4);
            let best_move_multiplier = BEST_MOVE_MULTIPLIERS[best_move_stability as usize];

            let eval_multiplier = (1.0 + (stability.best_eval - eval) as f32 / EVAL_MULTIPLIER_SCALE)
                .clamp(EVAL_MULTIPLIER_MIN, EVAL_MULTIPLIER_MAX);

            let adjusted = soft.mul_f32(best_move_multiplier * eval_multiplier);
            report.elapsed() > adjusted.min(*hard)
        };

        stability.last_best_move = best_move;
        stability.best_eval = stability.best_eval.max(eval);
        self.stability.set(stability);

        has_elapsed
    }
}

#[derive(Clone, Copy)]
struct SearchStability {
    last_best_move: Option<Move>,
    best_move_stability: u8,
    best_eval: i32,
}

impl Default for SearchStability {
    fn default() -> Self {
        Self {
            last_best_move: None,
            best_move_stability: 0,
            best_eval: EVAL_MIN,
        }
    }
}
