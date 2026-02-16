use crate::eval::EVAL_MATE_THRESHOLD;
use crate::r#move::Move;

pub const MIN_SIZE_MB: usize = 1;
pub const MAX_SIZE_MB: usize = 4096;
pub const DEFAULT_SIZE_MB: usize = 64;

pub struct TranspositionTable {
    entries: Vec<Entry>,
    capacity: usize,
    age: u8,
}

#[derive(Clone)]
pub struct Entry {
    key: u64,
    pub depth: u8,
    pub eval: i32,
    pub bound: Bound,
    pub mv: Option<Move>,
    age: u8,
}

#[derive(Clone)]
pub enum Bound {
    Exact,
    Lower,
    Upper,
}

impl TranspositionTable {
    pub fn new(size_mb: usize) -> Self {
        if !(MIN_SIZE_MB..=MAX_SIZE_MB).contains(&size_mb) {
            panic!("invalid transposition table size: {size_mb}mb");
        }

        let size_bytes = size_mb.saturating_mul(1024 * 1024);
        let capacity = size_bytes / std::mem::size_of::<Entry>();

        // We use the nearest lower power of two for capacity so that indexing can
        // use fast bitwise AND (key & (pow2 - 1)) instead of modulo, as in index()
        let pow2 = capacity.next_power_of_two() / 2;

        let entries = vec![
            Entry {
                key: 0,
                depth: 0,
                eval: 0,
                bound: Bound::Exact,
                mv: None,
                age: 0,
            };
            pow2
        ];

        Self {
            entries,
            capacity: pow2,
            age: 0,
        }
    }

    pub fn probe(&self, key: u64) -> Option<&Entry> {
        let entry = &self.entries[self.index(key)];
        if entry.key == key { Some(entry) } else { None }
    }

    pub fn store(&mut self, key: u64, depth: u8, eval: i32, bound: Bound, mv: Option<Move>) {
        let index = self.index(key);
        let entry = &mut self.entries[index];

        if depth >= entry.depth || entry.age != self.age {
            entry.key = key;
            entry.depth = depth;
            entry.eval = eval;
            entry.bound = bound;
            entry.mv = mv;
            entry.age = self.age;
        }
    }

    pub fn usage(&self) -> usize {
        // Assume the first 1000 entries are representative of the table.
        self.entries
            .iter()
            .take(1000)
            .filter(|entry| entry.age == self.age)
            .count()
    }

    pub fn clear(&mut self) {
        self.entries.fill(Entry {
            key: 0,
            depth: 0,
            eval: 0,
            bound: Bound::Exact,
            mv: None,
            age: 0,
        });
        self.age = 0;
    }

    pub fn age(&mut self) {
        self.age = self.age.wrapping_add(1);
    }

    #[inline]
    fn index(&self, key: u64) -> usize {
        key as usize & (self.capacity - 1)
    }
}

// Normalize an eval before storing it in the transposition table. For checkmate,
// offset by the current ply to reflect distance to mate from the root. Non-mate
// evals are returned unchanged.
#[inline]
pub fn eval_in(eval: i32, ply: u8) -> i32 {
    if eval >= EVAL_MATE_THRESHOLD {
        eval + ply as i32
    } else if eval <= -EVAL_MATE_THRESHOLD {
        eval - ply as i32
    } else {
        eval
    }
}

// Denormalize an eval from the transposition table. Reverses `eval_in()` by
// removing the ply offset from mate evals to recover the correct eval at the
// current node. Non-mate evals are returned unchanged.
#[inline]
pub fn eval_out(eval: i32, ply: u8) -> i32 {
    if eval >= EVAL_MATE_THRESHOLD {
        eval - ply as i32
    } else if eval <= -EVAL_MATE_THRESHOLD {
        eval + ply as i32
    } else {
        eval
    }
}
