mod attacks;
mod bench;
mod colour;
mod eval;
mod info;
mod movegen;
mod piece;
mod position;
mod rng;
mod search;
mod square;
mod uci;

#[cfg(test)]
mod testing;

fn main() {
    println!("{}, {}", info::name(), info::author());

    let args: Vec<_> = std::env::args().collect();

    match args.get(1).map(|s| s.as_str()) {
        Some("bench") => bench::run(&args[2..]),
        _ => uci::main(),
    }
}
