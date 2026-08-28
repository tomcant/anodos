use crate::colour::Colour;
use crate::square::FILES;
use crate::{squares, u64_list};

pub fn build() -> String {
    format!(
        "static PAWN_ATTACKS: [[u64; 64]; 2] = [{}, {}];\n",
        u64_list(&pawn_attacks(Colour::White)),
        u64_list(&pawn_attacks(Colour::Black)),
    )
}

#[rustfmt::skip]
fn pawn_attacks(colour: Colour) -> [u64; 64] {
    let mut attacks = [0; 64];

    for square in squares() {
        let square_u64 = square.u64();

        attacks[square] = match colour {
            Colour::White => (square_u64 & !FILES[0]) << 7 | (square_u64 & !FILES[7]) << 9,
            Colour::Black => (square_u64 & !FILES[7]) >> 7 | (square_u64 & !FILES[0]) >> 9,
        };
    }

    attacks
}
