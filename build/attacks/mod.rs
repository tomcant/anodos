mod king;
mod knight;
mod pawn;
mod sliders;

pub fn build() -> String {
    [sliders::build(), pawn::build(), knight::build(), king::build()].concat()
}
