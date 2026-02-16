use super::*;
use std::fmt::{Display, Formatter, Result};

impl Display for Position {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "{}\n  [{}] [Castling {}]\n  [EP {}] [HM {}] [FM {}]\n",
            self.board,
            if self.colour_to_move == Colour::White { "White" } else { "Black" },
            fen::castling_rights_to_fen(self.castling_rights),
            self.en_passant_square.map_or("-".to_string(), |s| s.to_string()),
            self.half_move_clock,
            self.full_move_counter
        )?;

        Ok(())
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "\n   -----------------")?;

        for rank in (0..8).rev() {
            let mut line = String::new();

            for file in 0..8 {
                let square = Square::from_file_and_rank(file, rank);

                match self.piece_at(square) {
                    Some(piece) => {
                        #[rustfmt::skip]
                        let symbol = match piece {
                            WP => 'P', WN => 'N', WB => 'B', WR => 'R', WQ => 'Q', WK => 'K',
                            BP => 'p', BN => 'n', BB => 'b', BR => 'r', BQ => 'q', BK => 'k',
                        };
                        line.push_str(&format!("{symbol} "));
                    }
                    None => line.push_str(". "),
                }
            }

            writeln!(f, "{} | {} |", rank + 1, line.trim_end())?;
        }

        writeln!(f, "   -----------------")?;
        writeln!(f, "    a b c d e f g h")?;

        Ok(())
    }
}
