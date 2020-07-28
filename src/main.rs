use crate::ai::{piece_strength, Turn, evaluate_side, evaluate_state, minimax_recursion};
use crate::engine::Piece;
use crate::engine::Board;

pub mod ai;
pub mod engine;

fn main() {
    let mut board = Board::init_board();
}