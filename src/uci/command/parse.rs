use super::{
    GoParams,
    UciCommand::{self, *},
};
use crate::position::{Position, START_POS_FEN};
use crate::search::tt;
use std::time::Duration;

impl std::str::FromStr for UciCommand {
    type Err = String;

    fn from_str(command: &str) -> Result<Self, Self::Err> {
        let parts: Vec<_> = command.split_whitespace().collect();
        let args = &parts[1..];

        match parts[0] {
            "uci" => Ok(Init),
            "isready" => Ok(IsReady),
            "ucinewgame" => Ok(NewGame),
            "printboard" => Ok(PrintBoard),
            "printfen" => Ok(PrintFen),
            "perft" => Ok(parse_perft(args)?),
            "domove" => Ok(parse_domove(args)?),
            "position" => Ok(parse_position(args)?),
            "go" => Ok(parse_go(args)?),
            "setoption" => Ok(parse_setoption(args)?),
            "stop" => Ok(Stop),
            "quit" => Ok(Quit),
            _ => Err(format!("unknown command '{}'", parts[0])),
        }
    }
}

fn parse_perft(args: &[&str]) -> Result<UciCommand, String> {
    if args.is_empty() {
        return Err("missing depth".to_string());
    }

    let depth = args[0]
        .parse()
        .map_err(|_| "could not parse value for depth".to_string())?;

    Ok(Perft(depth))
}

fn parse_domove(args: &[&str]) -> Result<UciCommand, String> {
    if args.is_empty() {
        return Err("missing move".to_string());
    }

    Ok(DoMove(args[0].parse()?))
}

fn parse_position(args: &[&str]) -> Result<UciCommand, String> {
    enum Token {
        None,
        Fen,
        Move,
    }

    let mut token = Token::None;
    let mut fen = String::from("");
    let mut moves = vec![];

    for arg in args {
        match *arg {
            "fen" => token = Token::Fen,
            "moves" => token = Token::Move,
            "startpos" => fen = START_POS_FEN.to_string(),

            _ => match token {
                Token::Fen => fen = format!("{fen} {arg}"),
                Token::Move => moves.push(arg.parse()?),
                _ => (),
            },
        }
    }

    fen.parse::<Position>()?;

    Ok(Position(fen.trim().to_string(), moves))
}

fn parse_go(args: &[&str]) -> Result<UciCommand, String> {
    let mut params = GoParams::new();
    let mut iter = args.iter();

    while let Some(attr) = iter.next() {
        if *attr == "infinite" {
            return Ok(Go(GoParams::new()));
        }

        let Some(value) = iter.next() else {
            return Err(format!("missing value for '{attr}' attribute"));
        };

        match *attr {
            "depth" => params.depth = Some(parse_u8_attr("depth", value)?),
            "movetime" => params.movetime = Some(parse_duration_attr("movetime", value)?),
            "wtime" => params.wtime = Some(parse_duration_attr("wtime", value)?),
            "btime" => params.btime = Some(parse_duration_attr("btime", value)?),
            "winc" => params.winc = Some(parse_duration_attr("winc", value)?),
            "binc" => params.binc = Some(parse_duration_attr("binc", value)?),
            "nodes" => params.nodes = Some(parse_u128_attr("nodes", value)?),
            _ => return Err(format!("unknown attribute '{attr}'")),
        }
    }

    Ok(Go(params))
}

fn parse_setoption(args: &[&str]) -> Result<UciCommand, String> {
    if args.is_empty() || args[0] != "name" {
        return Err("missing option name".to_string());
    }

    let mut name_parts: Vec<&str> = Vec::new();
    let mut value_parts: Vec<&str> = Vec::new();
    let mut in_value_section = false;

    for arg in &args[1..] {
        if *arg == "value" && !in_value_section {
            in_value_section = true;
            continue;
        }

        if in_value_section {
            value_parts.push(*arg);
        } else {
            name_parts.push(*arg);
        }
    }

    let name = name_parts.join(" ").trim().to_string().to_lowercase();
    let value = value_parts.join(" ").trim().to_string().to_lowercase();

    if name.is_empty() {
        return Err("missing option name".to_string());
    }

    match name.as_str() {
        "hash" => {
            if value.is_empty() {
                return Err("missing value for 'hash' option".to_string());
            };
            let Ok(size_mb) = value.parse::<usize>() else {
                return Err("could not parse value for 'hash' option".to_string());
            };
            if !(tt::MIN_SIZE_MB..=tt::MAX_SIZE_MB).contains(&size_mb) {
                return Err("invalid value for 'hash' option".to_string());
            };
            Ok(SetOption(name, Some(value)))
        }
        _ => Err(format!("unknown option '{name}'")),
    }
}

fn parse_u8_attr(attr: &str, value: &str) -> Result<u8, String> {
    value
        .parse::<u8>()
        .map_err(|_| format!("invalid value for '{attr}' attribute"))
}

fn parse_u128_attr(attr: &str, value: &str) -> Result<u128, String> {
    value
        .parse::<u128>()
        .map_err(|_| format!("invalid value for '{attr}' attribute"))
}

fn parse_duration_attr(attr: &str, value: &str) -> Result<Duration, String> {
    let ms = value
        .parse::<i128>()
        .map_err(|_| format!("invalid value for '{attr}' attribute"))?;

    Ok(Duration::from_millis(ms.max(0) as u64))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::piece::Piece::*;
    use crate::square::Square::*;
    use crate::uci::r#move::UciMove;

    #[test]
    fn parse_init_command() {
        assert_eq!("uci".parse(), Ok(Init));
    }

    #[test]
    fn parse_isready_command() {
        assert_eq!("isready".parse(), Ok(IsReady));
    }

    #[test]
    fn parse_ucinewgame_command() {
        assert_eq!("ucinewgame".parse(), Ok(NewGame));
    }

    #[test]
    fn parse_printboard_command() {
        assert_eq!("printboard".parse(), Ok(PrintBoard));
    }

    #[test]
    fn parse_printfen_command() {
        assert_eq!("printfen".parse(), Ok(PrintFen));
    }

    #[test]
    fn parse_perft_command() {
        assert_eq!("perft 1".parse(), Ok(Perft(1)));
    }

    #[test]
    fn parse_position_command_with_start_pos() {
        assert_eq!(
            "position startpos".parse(),
            Ok(Position(START_POS_FEN.to_string(), vec![]))
        );
    }

    #[test]
    fn parse_position_command_with_fen() {
        assert_eq!(
            "position fen 4k3/8/8/8/8/8/8/4K3 w - - 0 1".parse(),
            Ok(Position("4k3/8/8/8/8/8/8/4K3 w - - 0 1".to_string(), vec![]))
        );
    }

    #[test]
    fn parse_position_command_with_moves() {
        assert_eq!(
            "position startpos moves e2e4".parse(),
            Ok(Position(
                START_POS_FEN.to_string(),
                vec![UciMove {
                    from: E2,
                    to: E4,
                    promotion_piece: None
                }]
            ))
        );

        assert_eq!(
            "position fen 4k3/8/8/8/8/8/8/4K3 w - - 0 1 moves d2d4 e7e6".parse(),
            Ok(Position(
                "4k3/8/8/8/8/8/8/4K3 w - - 0 1".to_string(),
                vec![
                    UciMove {
                        from: D2,
                        to: D4,
                        promotion_piece: None
                    },
                    UciMove {
                        from: E7,
                        to: E6,
                        promotion_piece: None
                    }
                ]
            ))
        );

        assert_eq!(
            "position startpos moves e7e8q".parse(),
            Ok(Position(
                START_POS_FEN.to_string(),
                vec![UciMove {
                    from: E7,
                    to: E8,
                    promotion_piece: Some(WQ)
                }]
            ))
        );

        assert_eq!(
            "position startpos moves e2e1r".parse(),
            Ok(Position(
                START_POS_FEN.to_string(),
                vec![UciMove {
                    from: E2,
                    to: E1,
                    promotion_piece: Some(BR)
                }]
            ))
        );
    }

    #[test]
    fn parse_go_command() {
        assert_eq!(
            "go depth 1 movetime 2 wtime 3 btime 4 winc 5 binc 6 nodes 7".parse(),
            Ok(Go(GoParams {
                depth: Some(1),
                movetime: Some(Duration::from_millis(2)),
                wtime: Some(Duration::from_millis(3)),
                btime: Some(Duration::from_millis(4)),
                winc: Some(Duration::from_millis(5)),
                binc: Some(Duration::from_millis(6)),
                nodes: Some(7),
            }))
        );
    }

    #[test]
    fn parse_go_command_with_infinite_attribute() {
        assert_eq!(
            "go infinite".parse(),
            Ok(Go(GoParams {
                depth: None,
                movetime: None,
                wtime: None,
                btime: None,
                winc: None,
                binc: None,
                nodes: None,
            }))
        );
    }

    #[test]
    fn parse_setoption_command_with_hash_option() {
        assert_eq!(
            "setoption name Hash value 64".parse(),
            Ok(SetOption("hash".to_string(), Some("64".to_string())))
        );
    }

    #[test]
    fn parse_stop_command() {
        assert_eq!("stop".parse(), Ok(Stop));
    }

    #[test]
    fn parse_quit_command() {
        assert_eq!("quit".parse(), Ok(Quit));
    }
}
