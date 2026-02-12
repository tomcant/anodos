use crate::colour::Colour;
use crate::movegen::Move;
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

// Define the remaining squares for testing purposes
impl Square {
    pub const A2: Self = Self::from_index(8);
    pub const B2: Self = Self::from_index(9);
    pub const C2: Self = Self::from_index(10);
    pub const D2: Self = Self::from_index(11);
    pub const E2: Self = Self::from_index(12);
    pub const F2: Self = Self::from_index(13);
    pub const G2: Self = Self::from_index(14);
    pub const H2: Self = Self::from_index(15);

    pub const A3: Self = Self::from_index(16);
    pub const B3: Self = Self::from_index(17);
    pub const C3: Self = Self::from_index(18);
    pub const D3: Self = Self::from_index(19);
    pub const E3: Self = Self::from_index(20);
    pub const F3: Self = Self::from_index(21);
    pub const G3: Self = Self::from_index(22);
    pub const H3: Self = Self::from_index(23);

    pub const A4: Self = Self::from_index(24);
    pub const B4: Self = Self::from_index(25);
    pub const C4: Self = Self::from_index(26);
    pub const D4: Self = Self::from_index(27);
    pub const E4: Self = Self::from_index(28);
    pub const F4: Self = Self::from_index(29);
    pub const G4: Self = Self::from_index(30);
    pub const H4: Self = Self::from_index(31);

    pub const A5: Self = Self::from_index(32);
    pub const B5: Self = Self::from_index(33);
    pub const C5: Self = Self::from_index(34);
    pub const D5: Self = Self::from_index(35);
    pub const E5: Self = Self::from_index(36);
    pub const F5: Self = Self::from_index(37);
    pub const G5: Self = Self::from_index(38);
    pub const H5: Self = Self::from_index(39);

    pub const A6: Self = Self::from_index(40);
    pub const B6: Self = Self::from_index(41);
    pub const C6: Self = Self::from_index(42);
    pub const D6: Self = Self::from_index(43);
    pub const E6: Self = Self::from_index(44);
    pub const F6: Self = Self::from_index(45);
    pub const G6: Self = Self::from_index(46);
    pub const H6: Self = Self::from_index(47);

    pub const A7: Self = Self::from_index(48);
    pub const B7: Self = Self::from_index(49);
    pub const C7: Self = Self::from_index(50);
    pub const D7: Self = Self::from_index(51);
    pub const E7: Self = Self::from_index(52);
    pub const F7: Self = Self::from_index(53);
    pub const G7: Self = Self::from_index(54);
    pub const H7: Self = Self::from_index(55);
}
