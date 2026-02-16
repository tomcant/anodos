use super::MAX_DEPTH;
use crate::r#move::Move;
use crate::movegen::MoveList;

pub struct PvTable {
    table: [MoveList; MAX_DEPTH as usize + 1],
}

impl PvTable {
    pub fn new() -> Self {
        Self {
            table: std::array::from_fn(|_| MoveList::new()),
        }
    }

    pub fn root(&self) -> &MoveList {
        &self.table[0]
    }

    pub fn clear(&mut self, ply: u8) {
        self.table[ply as usize].clear();
    }

    pub fn update(&mut self, ply: u8, mv: Move) {
        let (parent, child) = self.table.split_at_mut((ply + 1) as usize);
        let pv = &mut parent[ply as usize];
        pv.clear();
        pv.push(mv);
        pv.append(&mut child[0]);
    }
}
