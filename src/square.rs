use crate::colour::Colour;

const FILE_A: u64 = 0x0101_0101_0101_0101;

const RANK_1: u64 = 0x0000_0000_0000_00FF;

const CORNERS: u64 = 0x8100_0000_0000_0081;

pub const LIGHT_SQUARES: u64 = 0x55AA_55AA_55AA_55AA;

pub const BACK_RANKS: u64 = RANK_1 | (RANK_1 << 56);

pub const FILES: [u64; 8] = [
    FILE_A,
    FILE_A << 1,
    FILE_A << 2,
    FILE_A << 3,
    FILE_A << 4,
    FILE_A << 5,
    FILE_A << 6,
    FILE_A << 7,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Square(u8);

impl Square {
    pub const A1: Self = Self(0);
    pub const B1: Self = Self(1);
    pub const C1: Self = Self(2);
    pub const D1: Self = Self(3);
    pub const E1: Self = Self(4);
    pub const F1: Self = Self(5);
    pub const G1: Self = Self(6);
    pub const H1: Self = Self(7);

    pub const A8: Self = Self(56);
    pub const B8: Self = Self(57);
    pub const C8: Self = Self(58);
    pub const D8: Self = Self(59);
    pub const E8: Self = Self(60);
    pub const F8: Self = Self(61);
    pub const G8: Self = Self(62);
    pub const H8: Self = Self(63);

    pub const fn from_index(index: u8) -> Self {
        Self(index)
    }

    pub fn from_file_and_rank(file: u8, rank: u8) -> Self {
        Self(rank << 3 | file)
    }

    pub fn first(squares: u64) -> Self {
        Self(squares.trailing_zeros() as u8)
    }

    pub fn last(squares: u64) -> Self {
        Self(63 - squares.leading_zeros() as u8)
    }

    pub fn next(squares: &mut u64) -> Self {
        let square = Self::first(*squares);
        *squares ^= square.u64();
        square
    }

    pub fn index(&self) -> u8 {
        self.0
    }

    pub fn u64(&self) -> u64 {
        1 << self.0
    }

    pub fn file(&self) -> u8 {
        self.0 & 7
    }

    pub fn rank(&self) -> u8 {
        self.0 >> 3
    }

    pub fn file_diff(&self, other: Square) -> u8 {
        self.file().abs_diff(other.file())
    }

    pub fn rank_diff(&self, other: Square) -> u8 {
        self.rank().abs_diff(other.rank())
    }

    pub fn advance(&self, colour: Colour) -> Self {
        match colour {
            Colour::White => Self(self.0 + 8),
            _ => Self(self.0 - 8),
        }
    }

    pub fn is_back_rank(&self) -> bool {
        self.u64() & BACK_RANKS != 0
    }

    pub fn is_corner(&self) -> bool {
        self.u64() & CORNERS != 0
    }
}

impl<T> std::ops::Index<Square> for [T; 64] {
    type Output = T;

    fn index(&self, square: Square) -> &Self::Output {
        &self[square.0 as usize]
    }
}

impl<T> std::ops::IndexMut<Square> for [T; 64] {
    fn index_mut(&mut self, square: Square) -> &mut Self::Output {
        &mut self[square.0 as usize]
    }
}

impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}", (b'a' + self.file()) as char, 1 + self.rank())
    }
}

impl std::str::FromStr for Square {
    type Err = String;

    fn from_str(square: &str) -> Result<Self, Self::Err> {
        if square.len() != 2 {
            return Err(format!("invalid square '{square}'"));
        }

        let chars: Vec<_> = square.chars().collect();

        let file = chars[0] as u8;
        let rank = chars[1] as u8;

        if !(b'a'..=b'h').contains(&file) || !(b'1'..=b'8').contains(&rank) {
            return Err(format!("invalid square '{square}'"));
        }

        Ok(Self::from_file_and_rank(file - b'a', rank - b'1'))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_from_a_file_and_a_rank() {
        assert_eq!(Square::from_file_and_rank(0, 0), Square::A1);
        assert_eq!(Square::from_file_and_rank(7, 7), Square::H8);
        assert_eq!(Square::from_file_and_rank(1, 4), Square::from_index(33));
    }

    #[test]
    fn create_from_algebraic_notation() {
        assert_eq!("a1".parse::<Square>(), Ok(Square::A1));
        assert_eq!("h8".parse::<Square>(), Ok(Square::H8));
        assert_eq!("b5".parse::<Square>(), Ok(Square::from_index(33)));
    }

    #[test]
    fn create_from_first_bit_in_a_bitboard() {
        let bitboard = Square::A1.u64() | Square::A8.u64();

        assert_eq!(Square::first(bitboard), Square::A1);
    }

    #[test]
    fn create_from_last_bit_in_a_bitboard() {
        let bitboard = Square::A1.u64() | Square::A8.u64();

        assert_eq!(Square::last(bitboard), Square::A8);
    }

    #[test]
    fn consume_next_bit_in_a_bitboard() {
        let mut bitboard = Square::A1.u64() | Square::A8.u64();

        assert_eq!(Square::next(&mut bitboard), Square::A1);
        assert_eq!(bitboard, Square::A8.u64());

        assert_eq!(Square::next(&mut bitboard), Square::A8);
        assert_eq!(bitboard, 0);
    }

    #[test]
    fn it_cannot_be_created_from_invalid_algebraic_notation() {
        for str in ["", "a", "a1b", "a9", "i1"] {
            assert!(str.parse::<Square>().is_err());
        }
    }

    #[test]
    fn advance_a_square_given_a_colour() {
        assert_eq!(Square::E5, Square::E4.advance(Colour::White));
        assert_eq!(Square::E3, Square::E4.advance(Colour::Black));
    }
}
