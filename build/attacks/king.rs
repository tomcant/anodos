use crate::square::FILES;
use crate::{squares, u64_list};

pub fn build() -> String {
    format!("static KING_ATTACKS: [u64; 64] = {};\n", u64_list(&king_attacks()))
}

#[rustfmt::skip]
fn king_attacks() -> [u64; 64] {
    let mut attacks = [0; 64];

    for square in squares() {
        let square_u64 = square.u64();

        attacks[square] =
              (square_u64 & !FILES[7]) << 1
            | (square_u64 & !FILES[0]) >> 1

            | square_u64 << 8
            | (square_u64 & !FILES[0]) << 7
            | (square_u64 & !FILES[7]) << 9

            | square_u64 >> 8
            | (square_u64 & !FILES[7]) >> 7
            | (square_u64 & !FILES[0]) >> 9;
    }

    attacks
}
