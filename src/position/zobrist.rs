use super::*;
use crate::attacks::en_passant_attacks;
use crate::piece::Piece;
use crate::square::Square;

// Include build-generated Zobrist keys
include!(concat!(env!("OUT_DIR"), "/zobrist.rs"));

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
