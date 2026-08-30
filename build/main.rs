// Build-time generator for the engine's lookup tables.
// Writes files into `OUT_DIR` for the engine to `include!(...)`.

#[path = "../src/colour.rs"]
mod colour;
#[path = "../src/piece.rs"]
mod piece;
#[allow(dead_code)]
#[path = "../src/square.rs"]
mod square;

mod attacks;
mod passed_pawns;
mod pawn_shields;
mod psqt;
mod rng;
mod zobrist;

use square::Square;
use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    for dep in ["colour.rs", "piece.rs", "square.rs"] {
        println!("cargo::rerun-if-changed=src/{dep}");
    }

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    for (file, contents) in [
        ("attacks.rs", attacks::build()),
        ("passed_pawns.rs", passed_pawns::build()),
        ("pawn_shields.rs", pawn_shields::build()),
        ("psqt.rs", psqt::build()),
        ("zobrist.rs", zobrist::build()),
    ] {
        fs::write(out_dir.join(file), contents).unwrap();
    }
}

fn squares() -> impl Iterator<Item = Square> {
    (0..64).map(Square::from_index)
}

fn u64_list(values: &[u64]) -> String {
    list(values.iter().map(|value| format!("{value:#018x}")))
}

fn i32_list(values: &[i32]) -> String {
    list(values.iter().map(|value| value.to_string()))
}

fn list(items: impl Iterator<Item = String>) -> String {
    format!("[{}]", items.collect::<Vec<_>>().join(", "))
}
