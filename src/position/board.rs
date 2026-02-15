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

    pub fn count_pieces(&self, piece: Piece) -> u32 {
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

    pub fn remove_piece(&mut self, square: Square, piece: Piece) {
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

    #[test]
    fn put_a_piece_on_the_board() {
        let mut board = Board::empty();
        let piece = Piece::WK;
        let square = Square::E1;

        board.put_piece(piece, square);

        assert!(board.has_piece_at(square));
        assert_eq!(board.piece_at(square), Some(piece));
        assert_eq!(board.count_pieces(piece), 1);
        assert_eq!(board.pieces_by_colour(piece.colour()) & square.u64(), square.u64());
    }

    #[test]
    fn remove_a_piece_from_the_board() {
        let mut board = Board::empty();
        let square = Square::E1;
        board.put_piece(Piece::WK, square);

        assert!(board.has_piece_at(square));

        board.remove_piece(square, Piece::WK);

        assert!(!board.has_piece_at(square));
    }
}
