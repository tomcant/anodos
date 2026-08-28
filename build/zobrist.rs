use crate::rng::{RNG_SEED, XorShift64};
use crate::square::Square;
use crate::{list, u64_list};

pub fn build() -> String {
    let mut rand = XorShift64::new(RNG_SEED);

    let mut piece_square = [[0; 64]; 12];
    for keys in piece_square.iter_mut() {
        for file in 0..8 {
            for rank in 0..8 {
                keys[Square::from_file_and_rank(file, rank)] = rand.next().unwrap();
            }
        }
    }

    let colour_to_move = rand.next().unwrap();

    let mut castling_rights = [0; 16];
    for right in castling_rights.iter_mut() {
        *right = rand.next().unwrap();
    }

    let mut en_passant_files = [0; 8];
    for file in en_passant_files.iter_mut() {
        *file = rand.next().unwrap();
    }

    let fields = [
        format!(
            "piece_square: {},",
            list(piece_square.iter().map(|values| u64_list(values)))
        ),
        format!("colour_to_move: {colour_to_move:#018x},"),
        format!("castling_rights: {},", u64_list(&castling_rights)),
        format!("en_passant_files: {},", u64_list(&en_passant_files)),
    ];

    format!("pub static ZOBRIST: Zobrist = Zobrist {{\n{}\n}};\n", fields.join("\n"))
}
