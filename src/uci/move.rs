use crate::colour::Colour;
use crate::r#move::Move;
use crate::piece::Piece;
use crate::square::Square;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UciMove {
    pub from: Square,
    pub to: Square,
    pub promotion_piece: Option<Piece>,
}

impl From<Move> for UciMove {
    fn from(m: Move) -> Self {
        UciMove {
            from: m.from,
            to: m.to,
            promotion_piece: m.promotion_piece,
        }
    }
}

impl std::str::FromStr for UciMove {
    type Err = String;

    fn from_str(mv: &str) -> Result<Self, Self::Err> {
        if mv.len() != 4 && mv.len() != 5 {
            return Err("invalid UCI move".to_string());
        }

        let from = mv[0..2].parse::<Square>()?;
        let to = mv[2..4].parse::<Square>()?;

        let promotion_piece = if mv.len() > 4 {
            let colour = match to.rank() {
                0 => Colour::Black,
                _ => Colour::White,
            };
            Some(match mv.chars().nth(4).unwrap() {
                'n' => Piece::knight(colour),
                'b' => Piece::bishop(colour),
                'r' => Piece::rook(colour),
                'q' => Piece::queen(colour),
                _ => return Err("invalid promotion piece in UCI move".to_string()),
            })
        } else {
            None
        };

        Ok(UciMove {
            from,
            to,
            promotion_piece,
        })
    }
}

impl std::fmt::Display for UciMove {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.from,
            self.to,
            if let Some(piece) = self.promotion_piece {
                format!("{piece}").to_lowercase()
            } else {
                String::from("")
            }
        )
    }
}
