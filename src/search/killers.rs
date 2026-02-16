use super::MAX_DEPTH;
use crate::r#move::Move;

pub struct KillerMoves {
    moves: [[Option<Move>; 2]; MAX_DEPTH as usize],
}

impl KillerMoves {
    pub fn new() -> Self {
        Self {
            moves: [[None; 2]; MAX_DEPTH as usize],
        }
    }

    pub fn probe(&self, ply: u8, index: usize) -> Option<Move> {
        self.moves[ply as usize][index]
    }

    pub fn is_killer(&self, ply: u8, mv: &Move) -> bool {
        self.moves[ply as usize].iter().any(|k| k.is_some_and(|k| *mv == k))
    }

    pub fn store(&mut self, ply: u8, mv: &Move) {
        let moves = &mut self.moves[ply as usize];

        if moves[0].is_none() || !mv.equals(&moves[0].unwrap()) {
            moves[1] = moves[0];
            moves[0] = Some(*mv);
        }
    }
}
