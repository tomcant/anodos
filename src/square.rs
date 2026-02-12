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

#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Square {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
}

impl Square {
    pub const fn from_index(index: u8) -> Self {
        debug_assert!(index < 64);
        unsafe { std::mem::transmute(index) }
    }

    pub fn from_file_and_rank(file: u8, rank: u8) -> Self {
        Self::from_index(rank << 3 | file)
    }

    pub fn first(squares: u64) -> Self {
        Self::from_index(squares.trailing_zeros() as u8)
    }

    pub fn last(squares: u64) -> Self {
        Self::from_index(63 - squares.leading_zeros() as u8)
    }

    pub fn next(squares: &mut u64) -> Self {
        let square = Self::first(*squares);
        *squares ^= square.u64();
        square
    }

    pub fn index(&self) -> u8 {
        *self as u8
    }

    pub fn u64(&self) -> u64 {
        1 << (*self as u8)
    }

    pub fn file(&self) -> u8 {
        *self as u8 & 7
    }

    pub fn rank(&self) -> u8 {
        *self as u8 >> 3
    }

    pub fn file_diff(&self, other: Square) -> u8 {
        self.file().abs_diff(other.file())
    }

    pub fn rank_diff(&self, other: Square) -> u8 {
        self.rank().abs_diff(other.rank())
    }

    pub fn advance(&self, colour: Colour) -> Self {
        match colour {
            Colour::White => Self::from_index(*self as u8 + 8),
            _ => Self::from_index(*self as u8 - 8),
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
        &self[square as usize]
    }
}

impl<T> std::ops::IndexMut<Square> for [T; 64] {
    fn index_mut(&mut self, square: Square) -> &mut Self::Output {
        &mut self[square as usize]
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
    use super::Square::{self, *};
    use crate::colour::Colour;

    #[test]
    fn create_from_a_file_and_a_rank() {
        assert_eq!(Square::from_file_and_rank(0, 0), A1);
        assert_eq!(Square::from_file_and_rank(7, 7), H8);
        assert_eq!(Square::from_file_and_rank(1, 4), Square::from_index(33));
    }

    #[test]
    fn create_from_algebraic_notation() {
        assert_eq!("a1".parse::<Square>(), Ok(A1));
        assert_eq!("h8".parse::<Square>(), Ok(H8));
        assert_eq!("b5".parse::<Square>(), Ok(Square::from_index(33)));
    }

    #[test]
    fn create_from_first_bit_in_a_bitboard() {
        let bitboard = A1.u64() | A8.u64();

        assert_eq!(Square::first(bitboard), A1);
    }

    #[test]
    fn create_from_last_bit_in_a_bitboard() {
        let bitboard = A1.u64() | A8.u64();

        assert_eq!(Square::last(bitboard), A8);
    }

    #[test]
    fn consume_next_bit_in_a_bitboard() {
        let mut bitboard = A1.u64() | A8.u64();

        assert_eq!(Square::next(&mut bitboard), A1);
        assert_eq!(bitboard, A8.u64());

        assert_eq!(Square::next(&mut bitboard), A8);
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
        assert_eq!(E5, E4.advance(Colour::White));
        assert_eq!(E3, E4.advance(Colour::Black));
    }
}
