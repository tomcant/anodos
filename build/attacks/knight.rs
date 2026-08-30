use crate::square::FILES;
use crate::{squares, u64_list};

pub fn build() -> String {
    format!("static KNIGHT_ATTACKS: [u64; 64] = {};\n", u64_list(&knight_attacks()))
}

#[rustfmt::skip]
fn knight_attacks() -> [u64; 64] {
    let mut attacks = [0; 64];

    for square in squares() {
        let square_u64 = square.u64();

        attacks[square] =
              (square_u64 & !FILES[0] & !FILES[1]) << 6  // up 1, left 2
            | (square_u64 & !FILES[6] & !FILES[7]) << 10 // up 1, right 2
            | (square_u64 & !FILES[0]) << 15             // up 2, left 1
            | (square_u64 & !FILES[7]) << 17             // up 2, right 1

            | (square_u64 & !FILES[6] & !FILES[7]) >> 6  // down 1, right 2
            | (square_u64 & !FILES[0] & !FILES[1]) >> 10 // down 1, left 2
            | (square_u64 & !FILES[7]) >> 15             // down 2, right 1
            | (square_u64 & !FILES[0]) >> 17;            // down 2, left 1
    }

    attacks
}
