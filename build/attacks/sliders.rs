use crate::rng::{RNG_SEED, XorShift64};
use crate::square::Square;

pub fn build() -> String {
    let mut out = "pub struct Magic { pub mask: u64, pub num: u64, pub shift: u8, pub offset: usize }\n".to_string();
    out.push_str(&build_magics("ROOK", &rook_mask, &rook_attacks));
    out.push_str(&build_magics("BISHOP", &bishop_mask, &bishop_attacks));
    out
}

fn build_magics(piece_name: &str, mask_fn: &dyn Fn(Square) -> u64, attacks_fn: &dyn Fn(Square, u64) -> u64) -> String {
    let mut out = format!("pub static {piece_name}_MAGICS: [Magic; 64] = [\n");
    let mut attacks = vec![];

    for index in 0..64 {
        let square = Square::from_index(index);
        let (mask, num, shift, table) = find_magic_for_square(square, mask_fn, attacks_fn);
        let offset = attacks.len();

        out.push_str(&format!(
            "    Magic {{ mask: {mask:#x}, num: {num:#x}, shift: {shift}, offset: {offset} }},\n",
        ));

        attacks.extend_from_slice(&table);
    }

    out.push_str("];\n");
    out.push_str(&format!(
        "pub static {piece_name}_ATTACKS: [u64; {}] = [\n",
        attacks.len()
    ));

    for attacks in &attacks {
        out.push_str(&format!("    {attacks:#x},\n"));
    }

    out.push_str("];\n");
    out
}

fn find_magic_for_square(
    square: Square,
    mask_fn: &dyn Fn(Square) -> u64,
    attacks_fn: &dyn Fn(Square, u64) -> u64,
) -> (u64, u64, u8, Vec<u64>) {
    let mask = mask_fn(square);
    let table_size = 1 << mask.count_ones();
    let bits = bit_positions(mask);

    let mut occupancies = Vec::with_capacity(table_size);
    let mut attacks = Vec::with_capacity(table_size);

    for index in 0..table_size {
        let occupancy = bit_permutation_from_index(index, &bits);
        occupancies.push(occupancy);
        attacks.push(attacks_fn(square, occupancy));
    }

    let mut rand = XorShift64::new(RNG_SEED);
    let shift = 64 - mask.count_ones() as u8;

    'search: loop {
        let candidate = rand.next_sparse();
        let mut table = vec![None; table_size];

        for i in 0..table_size {
            let index = ((occupancies[i].wrapping_mul(candidate)) >> shift) as usize;

            if table[index].is_some() {
                continue 'search; // Collision, try another magic number
            }

            table[index] = Some(attacks[i]);
        }

        let table = table.into_iter().map(|attacks| attacks.unwrap()).collect();

        return (mask, candidate, shift, table);
    }
}

fn rook_mask(square: Square) -> u64 {
    let mut mask = 0;
    let file = square.file();
    let rank = square.rank();

    // Up
    for r in (rank + 1)..7 {
        mask |= Square::from_file_and_rank(file, r).u64();
    }

    // Down
    for r in (1..rank).rev() {
        mask |= Square::from_file_and_rank(file, r).u64();
    }

    // Left
    for f in (1..file).rev() {
        mask |= Square::from_file_and_rank(f, rank).u64();
    }

    // Right
    for f in (file + 1)..7 {
        mask |= Square::from_file_and_rank(f, rank).u64();
    }

    mask
}

fn bishop_mask(square: Square) -> u64 {
    let mut mask = 0;
    let file = square.file();
    let rank = square.rank();

    // Up-right
    {
        let mut f = file + 1;
        let mut r = rank + 1;

        while f < 7 && r < 7 {
            mask |= Square::from_file_and_rank(f, r).u64();
            f += 1;
            r += 1;
        }
    }

    // Up-left
    {
        let mut f = file as i8 - 1;
        let mut r = rank + 1;

        while f >= 1 && r < 7 {
            mask |= Square::from_file_and_rank(f as u8, r).u64();
            f -= 1;
            r += 1;
        }
    }

    // Down-right
    {
        let mut f = file + 1;
        let mut r = rank as i8 - 1;

        while f < 7 && r >= 1 {
            mask |= Square::from_file_and_rank(f, r as u8).u64();
            f += 1;
            r -= 1;
        }
    }

    // Down-left
    {
        let mut f = file as i8 - 1;
        let mut r = rank as i8 - 1;

        while f >= 1 && r >= 1 {
            mask |= Square::from_file_and_rank(f as u8, r as u8).u64();
            f -= 1;
            r -= 1;
        }
    }

    mask
}

fn rook_attacks(square: Square, occupancy: u64) -> u64 {
    let mut attacks = 0;
    let file = square.file();
    let rank = square.rank();

    // Up
    for r in (rank + 1)..8 {
        let square = Square::from_file_and_rank(file, r).u64();
        attacks |= square;

        if occupancy & square != 0 {
            break;
        }
    }

    // Down
    for r in (0..rank as i8).rev() {
        let square = Square::from_file_and_rank(file, r as u8).u64();
        attacks |= square;

        if occupancy & square != 0 {
            break;
        }
    }

    // Left
    for f in (0..file as i8).rev() {
        let square = Square::from_file_and_rank(f as u8, rank).u64();
        attacks |= square;

        if occupancy & square != 0 {
            break;
        }
    }

    // Right
    for f in (file + 1)..8 {
        let square = Square::from_file_and_rank(f, rank).u64();
        attacks |= square;

        if occupancy & square != 0 {
            break;
        }
    }

    attacks
}

fn bishop_attacks(square: Square, occupancy: u64) -> u64 {
    let mut attacks = 0;
    let file = square.file();
    let rank = square.rank();

    // Up-right
    {
        let mut f = file + 1;
        let mut r = rank + 1;

        while f < 8 && r < 8 {
            let square = Square::from_file_and_rank(f, r).u64();
            attacks |= square;

            if occupancy & square != 0 {
                break;
            }

            f += 1;
            r += 1;
        }
    }

    // Up-left
    {
        let mut f = file as i8 - 1;
        let mut r = rank + 1;

        while f >= 0 && r < 8 {
            let square = Square::from_file_and_rank(f as u8, r).u64();
            attacks |= square;

            if occupancy & square != 0 {
                break;
            }

            f -= 1;
            r += 1;
        }
    }

    // Down-right
    {
        let mut f = file + 1;
        let mut r = rank as i8 - 1;

        while f < 8 && r >= 0 {
            let square = Square::from_file_and_rank(f, r as u8).u64();
            attacks |= square;

            if occupancy & square != 0 {
                break;
            }

            f += 1;
            r -= 1;
        }
    }

    // Down-left
    {
        let mut f = file as i8 - 1;
        let mut r = rank as i8 - 1;

        while f >= 0 && r >= 0 {
            let square = Square::from_file_and_rank(f as u8, r as u8).u64();
            attacks |= square;

            if occupancy & square != 0 {
                break;
            }

            f -= 1;
            r -= 1;
        }
    }

    attacks
}

fn bit_positions(mut bb: u64) -> Vec<u64> {
    let mut bits = vec![];

    while bb != 0 {
        let lsb = bb & bb.wrapping_neg();
        bits.push(lsb);
        bb ^= lsb;
    }

    bits
}

fn bit_permutation_from_index(index: usize, bits: &[u64]) -> u64 {
    let mut occupancy = 0;

    for (i, &bit) in bits.iter().enumerate() {
        if (index >> i) & 1 == 1 {
            occupancy |= bit;
        }
    }

    occupancy
}

impl XorShift64 {
    #[inline]
    fn next_sparse(&mut self) -> u64 {
        self.next().unwrap() & self.next().unwrap() & self.next().unwrap()
    }
}
