use crate::colour::Colour;
use crate::square::Square;
use crate::{list, squares};

pub fn build() -> String {
    format!(
        "static PAWN_SHIELDS: [[(u64, u64); 64]; 2] = [{}, {}];\n",
        shield_list(&pawn_shields(Colour::White)),
        shield_list(&pawn_shields(Colour::Black)),
    )
}

fn shield_list(masks: &[(u64, u64); 64]) -> String {
    list(masks.iter().map(|(close, far)| format!("({close:#018x}, {far:#018x})")))
}

fn pawn_shields(colour: Colour) -> [(u64, u64); 64] {
    let mut masks = [(0, 0); 64];

    for square in squares() {
        let rank = square.rank();

        let close_rank = match colour {
            Colour::White if rank < 7 => Some(rank + 1),
            Colour::Black if rank > 0 => Some(rank - 1),
            _ => None,
        };
        let far_rank = match colour {
            Colour::White if rank < 6 => Some(rank + 2),
            Colour::Black if rank > 1 => Some(rank - 2),
            _ => None,
        };

        let (mut close, mut far) = (0, 0);

        for diff in [-1, 0, 1] {
            let file = square.file() as i8 + diff;

            if (0..8).contains(&file) {
                if let Some(rank) = close_rank {
                    close |= Square::from_file_and_rank(file as u8, rank).u64();
                }
                if let Some(rank) = far_rank {
                    far |= Square::from_file_and_rank(file as u8, rank).u64();
                }
            }
        }

        masks[square] = (close, far);
    }

    masks
}
