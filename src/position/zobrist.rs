use super::*;
use crate::attacks::en_passant_attacks;
use crate::piece::Piece;
use crate::rng::XorShift64;
use crate::square::Square;
use lazy_static::lazy_static;

// https://en.wikipedia.org/wiki/Hash_function#Fibonacci_hashing
const RNG_SEED: u64 = 0x9E3779B97F4A7C15;

impl Position {
    pub fn compute_key(&self) -> u64 {
        let mut key = 0;

        for piece in Piece::pieces() {
            let mut bitboard = self.board.pieces(*piece);
            while bitboard != 0 {
                key ^= ZOBRIST.piece_square[*piece][Square::next(&mut bitboard)];
            }
        }

        if self.colour_to_move == Colour::Black {
            key ^= ZOBRIST.colour_to_move;
        }

        key ^= ZOBRIST.castling_rights[self.castling_rights];

        if let Some(square) = self.en_passant_square
            && en_passant_attacks(square, self.colour_to_move, &self.board) != 0
        {
            key ^= ZOBRIST.en_passant_files[square.file() as usize];
        }

        key
    }
}

pub struct Zobrist {
    pub piece_square: [[u64; 64]; 12],
    pub colour_to_move: u64,
    pub castling_rights: [u64; 16],
    pub en_passant_files: [u64; 8],
}

lazy_static! {
    pub static ref ZOBRIST: Zobrist = {
        let mut rand = XorShift64::new(RNG_SEED);

        let mut piece_square = [[0; 64]; 12];
        for piece in Piece::pieces() {
            for file in 0..8 {
                for rank in 0..8 {
                    piece_square[*piece][Square::from_file_and_rank(file, rank)] = rand.next().unwrap();
                }
            }
        }

        let colour_to_move = rand.next().unwrap();

        let mut castling_rights = [0; 16];
        for right in castling_rights.iter_mut() {
            *right = rand.next().unwrap();
        }

        let mut en_passant_files = [0; 8];
        for file in en_passant_files.iter_mut() {
            *file = rand.next().unwrap();
        }

        Zobrist {
            piece_square,
            colour_to_move,
            castling_rights,
            en_passant_files,
        }
    };
}
