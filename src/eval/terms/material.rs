use super::EvalTerm;
use crate::colour::Colour;
use crate::piece::Piece;
use crate::position::Board;

pub const PIECE_WEIGHTS: [i32; 12] = [100, 300, 350, 500, 900, 0, 100, 300, 350, 500, 900, 0];

pub fn eval(colour: Colour, board: &Board) -> EvalTerm {
    let score = Piece::pieces_by_colour(colour)
        .iter()
        .fold(0, |acc, piece| acc + PIECE_WEIGHTS[*piece] * board.count(*piece) as i32);

    EvalTerm::unphased(score)
}
