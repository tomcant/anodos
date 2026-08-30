use super::EvalTerm;
use crate::colour::Colour;
use crate::piece::Piece;
use crate::position::Board;
use crate::square::Square;

// Include build-generated pawn shield masks
include!(concat!(env!("OUT_DIR"), "/pawn_shields.rs"));

const PAWN_SHIELD_CLOSE: i32 = 12;
const PAWN_SHIELD_FAR: i32 = 6;

pub fn eval(colour: Colour, board: &Board) -> EvalTerm {
    let king_square = Square::first(board.pieces(Piece::king(colour)));

    // Evaluate pawn shields when the king is out of the centre.
    if (3..5).contains(&king_square.file()) {
        return EvalTerm::zero();
    }

    let (close, far) = PAWN_SHIELDS[colour][king_square];
    let pawns = board.pieces(Piece::pawn(colour));
    let close_pawns = (pawns & close).count_ones() as i32;
    let far_pawns = (pawns & far).count_ones() as i32;

    EvalTerm::new(close_pawns * PAWN_SHIELD_CLOSE + far_pawns * PAWN_SHIELD_FAR, 0)
}
