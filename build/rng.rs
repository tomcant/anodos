// Random number generator used for deterministic table generation (Zobrist, magics)

// https://en.wikipedia.org/wiki/Hash_function#Fibonacci_hashing
pub const RNG_SEED: u64 = 0x9E3779B97F4A7C15;

pub struct XorShift64(u64);

impl XorShift64 {
    pub const fn new(seed: u64) -> Self {
        Self(seed)
    }
}

impl Iterator for XorShift64 {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        // https://en.wikipedia.org/wiki/Xorshift
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        Some(x)
    }
}
