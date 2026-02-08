use super::EvalTerm;
use crate::attacks::attacks;
use crate::colour::Colour;
use crate::piece::Piece;
use crate::position::Board;
use crate::square::Square;

const KNIGHT_WEIGHTS: (i32, i32) = (4, 4);
const BISHOP_WEIGHTS: (i32, i32) = (4, 4);
const ROOK_WEIGHTS: (i32, i32) = (2, 3);
const QUEEN_WEIGHTS: (i32, i32) = (1, 2);

pub fn eval(colour: Colour, board: &Board) -> EvalTerm {
    let occupancy = board.pieces_by_colour(colour);

    let knights = mobility(Piece::knight(colour), KNIGHT_WEIGHTS, occupancy, board);
    let bishops = mobility(Piece::bishop(colour), BISHOP_WEIGHTS, occupancy, board);
    let rooks = mobility(Piece::rook(colour), ROOK_WEIGHTS, occupancy, board);
    let queens = mobility(Piece::queen(colour), QUEEN_WEIGHTS, occupancy, board);

    knights + bishops + rooks + queens
}

#[inline(always)]
fn mobility(piece: Piece, weights: (i32, i32), occupancy: u64, board: &Board) -> EvalTerm {
    let (mut mg, mut eg) = (0, 0);
    let mut pieces = board.pieces(piece);

    while pieces != 0 {
        let square = Square::next(&mut pieces);
        let attacks = attacks(piece, square, board) & !occupancy;
        let mobility = attacks.count_ones() as i32;

        mg += mobility * weights.0;
        eg += mobility * weights.1;
    }

    EvalTerm::new(mg, eg)
}
