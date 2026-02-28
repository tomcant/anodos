use super::*;

pub const START_POS_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

impl Position {
    pub fn to_fen(&self) -> String {
        let board = board_to_fen(&self.board);
        let colour = match self.colour_to_move {
            Colour::White => 'w',
            _ => 'b',
        };
        let castling = castling_rights_to_fen(self.castling_rights);
        let en_passant = match self.en_passant_square {
            Some(square) => square.to_string(),
            None => "-".to_string(),
        };

        format!(
            "{board} {colour} {castling} {en_passant} {} {}",
            self.half_move_clock, self.full_move_counter
        )
    }
}

fn board_to_fen(board: &Board) -> String {
    let mut output = String::new();

    for rank in (0..8).rev() {
        let mut empty_run = 0u8;

        for file in 0..8 {
            let square = Square::from_file_and_rank(file, rank);

            match board.piece_at(square) {
                Some(piece) => {
                    if empty_run > 0 {
                        output.push((b'0' + empty_run) as char);
                        empty_run = 0;
                    }
                    output.push_str(&piece.to_string());
                }
                None => {
                    empty_run += 1;
                }
            }
        }

        if empty_run > 0 {
            output.push((b'0' + empty_run) as char);
        }

        if rank > 0 {
            output.push('/');
        }
    }

    output
}

pub fn castling_rights_to_fen(rights: CastlingRights) -> String {
    let mut output = String::new();

    if rights.has(CastlingRight::WhiteKing) {
        output.push('K');
    }
    if rights.has(CastlingRight::WhiteQueen) {
        output.push('Q');
    }
    if rights.has(CastlingRight::BlackKing) {
        output.push('k');
    }
    if rights.has(CastlingRight::BlackQueen) {
        output.push('q');
    }

    if output.is_empty() {
        output.push('-');
    }

    output
}

impl std::str::FromStr for Position {
    type Err = String;

    fn from_str(fen: &str) -> Result<Self, Self::Err> {
        let parts: Vec<_> = fen.split_whitespace().collect();
        let num_parts = parts.len();

        if num_parts < 4 {
            return Err(format!("FEN must contain at least 4 parts, got {num_parts}"));
        }

        let half_move_clock = if num_parts > 4 { parts[4].parse().unwrap() } else { 0 };
        let full_move_counter = if num_parts > 5 { parts[5].parse().unwrap() } else { 1 };

        Ok(Position::new(
            parse_board(parts[0])?,
            parse_colour_to_move(parts[1])?,
            parse_castling_rights(parts[2])?,
            parse_en_passant_square(parts[3])?,
            half_move_clock,
            full_move_counter,
        ))
    }
}

fn parse_board(str: &str) -> Result<Board, String> {
    let row_count = str.matches('/').count() + 1;

    if row_count != 8 {
        return Err(format!("board must contain 8 rows, got {}", row_count));
    }

    let mut board = Board::empty();
    let mut square_index = A8.index();

    for char in str.chars() {
        if char == '/' {
            square_index -= 16;
            continue;
        }

        if char.is_ascii_digit() {
            square_index += char as u8 - b'0';
            continue;
        }

        #[rustfmt::skip]
        let piece = match char {
            'P' => WP, 'N' => WN, 'B' => WB, 'R' => WR, 'Q' => WQ, 'K' => WK,
            'p' => BP, 'n' => BN, 'b' => BB, 'r' => BR, 'q' => BQ, 'k' => BK,
            _ => return Err(format!("invalid piece '{char}'")),
        };

        board.put_piece(piece, Square::from_index(square_index));
        square_index += 1;
    }

    if square_index != 8 {
        return Err("board must contain 64 squares".to_string());
    }

    Ok(board)
}

fn parse_colour_to_move(colour: &str) -> Result<Colour, String> {
    match colour {
        "w" => Ok(Colour::White),
        "b" => Ok(Colour::Black),
        _ => Err(format!("invalid colour to move '{colour}'")),
    }
}

fn parse_castling_rights(str: &str) -> Result<CastlingRights, String> {
    if str == "-" {
        return Ok(CastlingRights::none());
    }

    let mut rights = CastlingRights::none();

    for char in str.chars() {
        rights.add(match char {
            'K' => CastlingRight::WhiteKing,
            'Q' => CastlingRight::WhiteQueen,
            'k' => CastlingRight::BlackKing,
            'q' => CastlingRight::BlackQueen,
            _ => return Err("invalid castling rights".to_string()),
        });
    }

    Ok(rights)
}

fn parse_en_passant_square(square: &str) -> Result<Option<Square>, String> {
    if square == "-" {
        return Ok(None);
    }

    let result = square.parse::<Square>();

    if result.is_err() {
        return Err("invalid en passant square".to_string());
    }

    let square = result.unwrap();

    if square.rank() != 2 && square.rank() != 5 {
        return Err("invalid en passant square".to_string());
    }

    Ok(Some(square))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_a_valid_fen() {
        let parse = START_POS_FEN.parse::<Position>();

        assert!(parse.is_ok());
        assert_eq!(parse.unwrap().board.piece_at(E2), Some(WP));
    }

    #[test]
    fn parse_error_with_too_few_parts() {
        assert_parse_error("w - -", "FEN must contain at least 4 parts, got 3");
    }

    #[test]
    fn parse_error_with_wrong_number_of_rows() {
        assert_parse_error("8/8 w - -", "board must contain 8 rows, got 2");
        assert_parse_error("8/8/8/8/8/8/8/8/1 w - -", "board must contain 8 rows, got 9");
    }

    #[test]
    fn parse_error_with_wrong_number_of_squares() {
        assert_parse_error("8/8/8/8/8/8/8/7 w - -", "board must contain 64 squares");
        assert_parse_error("8/8/8/8/8/8/8/9 w - -", "board must contain 64 squares");
    }

    #[test]
    fn parse_error_with_invalid_piece() {
        assert_parse_error("8/8/8/8/8/8/8/4a3 w - -", "invalid piece 'a'");
    }

    #[test]
    fn parse_with_white_to_move() {
        let parse = "8/8/8/8/8/8/8/8 w - -".parse::<Position>();

        assert!(parse.is_ok());
        assert_eq!(parse.unwrap().colour_to_move, Colour::White);
    }

    #[test]
    fn parse_with_black_to_move() {
        let parse = "8/8/8/8/8/8/8/8 b - -".parse::<Position>();

        assert!(parse.is_ok());
        assert_eq!(parse.unwrap().colour_to_move, Colour::Black);
    }

    #[test]
    fn parse_error_with_invalid_colour_to_move() {
        assert_parse_error("8/8/8/8/8/8/8/8 W - -", "invalid colour to move 'W'");
    }

    #[test]
    fn parse_with_no_castling_rights() {
        let parse = "8/8/8/8/8/8/8/8 w - -".parse::<Position>();

        assert!(parse.is_ok());
        assert_eq!(parse.unwrap().castling_rights, CastlingRights::none());
    }

    #[test]
    fn parse_with_partial_castling_rights() {
        let parse = "8/8/8/8/8/8/8/8 w Kq -".parse::<Position>();

        assert!(parse.is_ok());

        assert_eq!(
            parse.unwrap().castling_rights,
            CastlingRights::from(&[CastlingRight::WhiteKing, CastlingRight::BlackQueen])
        );
    }

    #[test]
    fn parse_with_all_castling_rights() {
        let parse = "8/8/8/8/8/8/8/8 w KQkq -".parse::<Position>();

        assert!(parse.is_ok());
        assert_eq!(parse.unwrap().castling_rights, CastlingRights::all());
    }

    #[test]
    fn parse_error_with_invalid_castling_rights() {
        assert_parse_error("8/8/8/8/8/8/8/8 w K- -", "invalid castling rights");
    }

    #[test]
    fn parse_with_no_en_passant_square() {
        let parse = "8/8/8/8/8/8/8/8 w - -".parse::<Position>();

        assert!(parse.is_ok());
        assert_eq!(parse.unwrap().en_passant_square, None);
    }

    #[test]
    fn parse_with_en_passant_square_3rd_rank() {
        let parse = "8/8/8/8/8/8/8/8 w - f3".parse::<Position>();

        assert!(parse.is_ok());
        assert_eq!(parse.unwrap().en_passant_square, Some(F3));
    }

    #[test]
    fn parse_with_en_passant_square_6th_rank() {
        let parse = "8/8/8/8/8/8/8/8 w - f6".parse::<Position>();

        assert!(parse.is_ok());
        assert_eq!(parse.unwrap().en_passant_square, Some(F6));
    }

    #[test]
    fn parse_error_with_invalid_en_passant_square() {
        assert_parse_error("8/8/8/8/8/8/8/8 w - f4", "invalid en passant square");
    }

    #[test]
    fn parse_with_move_counters() {
        let parse = "8/8/8/8/8/8/8/8 w - - 10 20".parse::<Position>();

        assert!(parse.is_ok());

        let pos = parse.unwrap();
        assert_eq!(pos.half_move_clock, 10);
        assert_eq!(pos.full_move_counter, 20);
    }

    fn assert_parse_error(fen: &str, err: &str) {
        let parse = fen.parse::<Position>();

        assert!(parse.is_err());
        assert_eq!(parse.unwrap_err(), err.to_string());
    }
}
