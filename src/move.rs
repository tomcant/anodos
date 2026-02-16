use crate::piece::Piece;
use crate::square::Square;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Move {
    pub piece: Piece,
    pub from: Square,
    pub to: Square,
    pub captured_piece: Option<Piece>,
    pub promotion_piece: Option<Piece>,
    pub is_en_passant: bool,
}

impl Move {
    pub fn capture_square(&self) -> Option<Square> {
        let captured_piece = self.captured_piece?;

        if self.is_en_passant {
            Some(self.to.advance(captured_piece.colour()))
        } else {
            Some(self.to)
        }
    }

    pub fn is_quiet(&self) -> bool {
        self.captured_piece.is_none() && self.promotion_piece.is_none()
    }

    pub fn is_castling(&self) -> bool {
        self.piece.is_king() && self.file_diff() > 1
    }

    pub fn file_diff(&self) -> u8 {
        self.from.file_diff(self.to)
    }

    pub fn rank_diff(&self) -> u8 {
        self.from.rank_diff(self.to)
    }

    // Compare only the parts of the move that define its semantics on the board.
    // Ignore transient state which can differ between sibling nodes at the same
    // ply, preventing heuristic matches.
    pub fn equals(&self, other: &Self) -> bool {
        self.piece == other.piece
            && self.from == other.from
            && self.to == other.to
            && self.promotion_piece == other.promotion_piece
            && self.is_en_passant == other.is_en_passant
    }
}
