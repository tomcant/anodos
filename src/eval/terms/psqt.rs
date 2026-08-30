use super::EvalTerm;
use crate::colour::Colour;
use crate::piece::Piece;
use crate::position::Board;
use crate::square::Square;

// Include build-generated piece square tables
include!(concat!(env!("OUT_DIR"), "/psqt.rs"));

#[inline(always)]
pub fn eval(colour: Colour, board: &Board) -> EvalTerm {
    let king_square = Square::first(board.pieces(Piece::king(colour)));
    let king_term = EvalTerm::new(PSQT_MG_KING[colour][king_square], PSQT_EG_KING[colour][king_square]);

    EvalTerm::unphased(eval_non_king(colour, board)) + king_term
}

#[inline(always)]
fn eval_non_king(colour: Colour, board: &Board) -> i32 {
    let pieces = [
        Piece::pawn(colour),
        Piece::knight(colour),
        Piece::bishop(colour),
        Piece::rook(colour),
        Piece::queen(colour),
    ];
    pieces.iter().fold(0, |mut acc, piece| {
        let mut pieces = board.pieces(*piece);
        while pieces != 0 {
            acc += PSQT_NON_KING[*piece][Square::next(&mut pieces)];
        }
        acc
    })
}
