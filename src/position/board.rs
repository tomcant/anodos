use crate::colour::Colour;
use crate::piece::Piece;
use crate::square::Square;

#[derive(Debug, Clone)]
pub struct Board {
    squares: [Option<Piece>; 64],
    pieces: [u64; 12],
    colours: [u64; 2],
}

impl Board {
    pub fn empty() -> Self {
        Self {
            squares: [None; 64],
            pieces: [0; 12],
            colours: [0; 2],
        }
    }

    pub fn pieces(&self, piece: Piece) -> u64 {
        self.pieces[piece]
    }

    pub fn pieces_by_colour(&self, colour: Colour) -> u64 {
        self.colours[colour]
    }

    pub fn count(&self, piece: Piece) -> u32 {
        self.pieces[piece].count_ones()
    }

    pub fn put_piece(&mut self, piece: Piece, square: Square) {
        self.squares[square] = Some(piece);
        self.pieces[piece] |= square.u64();
        self.colours[piece.colour()] |= square.u64();
    }

    pub fn piece_at(&self, square: Square) -> Option<Piece> {
        self.squares[square]
    }

    pub fn has_piece_at(&self, square: Square) -> bool {
        self.piece_at(square).is_some()
    }

    pub fn remove_piece(&mut self, square: Square) {
        let Some(piece) = self.piece_at(square) else {
            return;
        };
        self.squares[square] = None;
        self.pieces[piece] &= !square.u64();
        self.colours[piece.colour()] &= !square.u64();
    }

    pub fn occupancy(&self) -> u64 {
        self.colours[Colour::White] | self.colours[Colour::Black]
    }

    pub fn has_occupancy_at(&self, squares: u64) -> bool {
        self.occupancy() & squares != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::piece::Piece::*;
    use crate::square::Square::*;

    #[test]
    fn put_a_piece_on_the_board() {
        let mut board = Board::empty();

        board.put_piece(WK, E1);

        assert!(board.has_piece_at(E1));
        assert_eq!(board.piece_at(E1), Some(WK));
        assert_eq!(board.count(WK), 1);
        assert_eq!(board.pieces_by_colour(Colour::White) & E1.u64(), E1.u64());
    }

    #[test]
    fn remove_a_piece_from_the_board() {
        let mut board = Board::empty();
        board.put_piece(WK, E1);

        assert!(board.has_piece_at(E1));

        board.remove_piece(E1);

        assert!(!board.has_piece_at(E1));
    }
}
