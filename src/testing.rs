use crate::colour::Colour;
use crate::r#move::Move;
use crate::piece::Piece;
use crate::position::Position;
use crate::square::Square;

pub fn parse_fen(str: &str) -> Position {
    let pos = str.parse();
    assert!(pos.is_ok());
    pos.unwrap()
}

pub fn make_move(piece: Piece, from: Square, to: Square, captured_piece: Option<Piece>) -> Move {
    Move {
        piece,
        from,
        to,
        captured_piece,
        promotion_piece: None,
        is_en_passant: false,
    }
}

pub fn make_promotion_move(colour: Colour, from: Square, to: Square, piece: Piece) -> Move {
    Move {
        piece: Piece::pawn(colour),
        from,
        to,
        captured_piece: None,
        promotion_piece: Some(piece),
        is_en_passant: false,
    }
}

pub fn make_en_passant_move(colour: Colour, from: Square, to: Square) -> Move {
    Move {
        piece: Piece::pawn(colour),
        from,
        to,
        captured_piece: Some(Piece::pawn(colour.flip())),
        promotion_piece: None,
        is_en_passant: true,
    }
}
