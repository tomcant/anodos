use crate::info;
use crate::r#move::Move;
use crate::perft;
use crate::piece::Piece;
use crate::position::Position;
use crate::search::{
    search,
    stopper::Stopper,
    tt::{self, TranspositionTable},
};
use crate::uci::{r#move::UciMove, reporter::UciReporter};
use std::time::Instant;

pub fn init() {
    println!("id name {}", info::name());
    println!("id author {}", info::author());
    println!(
        "option name Hash type spin default {} min {} max {}",
        tt::DEFAULT_SIZE_MB,
        tt::MIN_SIZE_MB,
        tt::MAX_SIZE_MB
    );
    println!("uciok");
}

pub fn is_ready() {
    println!("readyok");
}

pub fn new_game(pos: &mut Position, tt: &mut TranspositionTable) {
    *pos = Position::startpos();
    tt.clear();
}

pub fn print_board(pos: &Position) {
    println!("{pos}");
}

pub fn print_fen(pos: &Position) {
    println!("{}", pos.to_fen());
}

pub fn perft(depth: u8, pos: &Position) {
    let started_at = Instant::now();
    let nodes = perft::divide(&mut pos.clone(), depth);
    let elapsed = started_at.elapsed();
    let ms = elapsed.as_millis();
    let nps = nodes * 1000 / ms.max(1);

    println!("\nnodes: {nodes}");
    println!("time: {ms} ms");
    println!("nps: {nps}\n");
}

pub fn do_move(mv: UciMove, pos: &mut Position) {
    let piece = pos.board.piece_at(mv.from).unwrap();
    let is_en_passant = piece.is_pawn() && pos.en_passant_square == Some(mv.to);

    let captured_piece = if is_en_passant {
        Some(Piece::pawn(pos.colour_to_move.flip()))
    } else {
        pos.board.piece_at(mv.to)
    };

    pos.do_move(&Move {
        piece,
        from: mv.from,
        to: mv.to,
        captured_piece,
        promotion_piece: mv.promotion_piece,
        is_en_passant,
    });
}

pub fn position(fen: String, moves: Vec<UciMove>, pos: &mut Position) {
    *pos = fen.parse().unwrap();

    for mv in moves {
        do_move(mv, pos);
    }
}

pub fn go(pos: &mut Position, tt: &mut TranspositionTable, stopper: &Stopper) {
    let reporter = UciReporter::new();
    search(pos, tt, &reporter, stopper);

    match reporter.best_move() {
        Some(mv) => println!("bestmove {mv}"),
        None => println!("bestmove (none)"),
    }
}

pub fn set_option(name: String, value: Option<String>, tt: &mut TranspositionTable) {
    match name.as_str() {
        "hash" => {
            let size_mb = value.unwrap().parse().unwrap();
            *tt = TranspositionTable::new(size_mb);
        }
        _ => panic!("unknown option '{name}'"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::piece::Piece::*;
    use crate::square::Square::*;
    use crate::uci::command::UciCommand::{self, *};

    #[test]
    fn handle_position_command_with_moves() {
        let command = "position startpos moves e2e4 e7e5";
        let Position(fen, moves) = parse_command(command) else {
            panic!()
        };
        let Ok(mut pos) = fen.parse() else { panic!() };

        position(fen, moves, &mut pos);

        assert_eq!(pos.board.piece_at(E4), Some(WP));
        assert_eq!(pos.board.piece_at(E5), Some(BP));
    }

    #[test]
    fn handle_position_command_with_promotion_moves() {
        let command = "position fen 8/1P2k3/8/8/8/8/4K1p1/8 w - - 0 1 moves b7b8q g2g1r";
        let Position(fen, moves) = parse_command(command) else {
            panic!()
        };
        let Ok(mut pos) = fen.parse() else { panic!() };

        position(fen, moves, &mut pos);

        assert_eq!(pos.board.piece_at(B8), Some(WQ));
        assert_eq!(pos.board.piece_at(G1), Some(BR));
    }

    #[test]
    fn handle_position_command_with_en_passant_move_for_white() {
        let command = "position fen 4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1 moves e5d6";
        let Position(fen, moves) = parse_command(command) else {
            panic!()
        };
        let Ok(mut pos) = fen.parse() else { panic!() };

        position(fen, moves, &mut pos);

        assert_eq!(pos.board.piece_at(D6), Some(WP));
        assert!(!pos.board.has_piece_at(E5));
        assert!(!pos.board.has_piece_at(D5));
    }

    #[test]
    fn handle_position_command_with_en_passant_move_for_black() {
        let command = "position fen 4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 1 moves d4e3";
        let Position(fen, moves) = parse_command(command) else {
            panic!()
        };
        let Ok(mut pos) = fen.parse() else { panic!() };

        position(fen, moves, &mut pos);

        assert_eq!(pos.board.piece_at(E3), Some(BP));
        assert!(!pos.board.has_piece_at(D4));
        assert!(!pos.board.has_piece_at(E4));
    }

    fn parse_command(str: &str) -> UciCommand {
        let command = str.parse();
        assert!(command.is_ok());
        command.unwrap()
    }
}
