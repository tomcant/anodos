use crate::colour::Colour;
use crate::square::{ADJACENT_FILES, FILES};
use crate::{squares, u64_list};

pub fn build() -> String {
    format!(
        "static PASSED_PAWN_MASKS: [[u64; 64]; 2] = [{}, {}];\n",
        u64_list(&passed_pawn_masks(Colour::White)),
        u64_list(&passed_pawn_masks(Colour::Black)),
    )
}

fn passed_pawn_masks(colour: Colour) -> [u64; 64] {
    let mut masks = [0; 64];

    for square in squares() {
        let file = square.file() as usize;
        let rank = square.rank() as u32;

        let ranks_in_front = match colour {
            Colour::White => {
                if rank < 7 {
                    !((1 << ((rank + 1) << 3)) - 1)
                } else {
                    0
                }
            }
            _ => {
                if rank > 0 {
                    (1 << (rank << 3)) - 1
                } else {
                    0
                }
            }
        };

        masks[square] = (FILES[file] | ADJACENT_FILES[file]) & ranks_in_front;
    }

    masks
}
