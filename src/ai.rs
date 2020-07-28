use crate::engine::Piece::*;
use crate::engine::Piece;
use crate::engine::Board;
use std::collections::HashSet;
use crate::engine::Move;
use wasm_bindgen::prelude::*;


#[derive(Eq, PartialEq)]
pub enum Turn {
    White,
    Black
}

pub fn piece_strength(piece : Piece) -> i32 {
    match piece {
        WhitePawn | BlackPawn => {
            10
        },
        WhiteKnight | BlackKnight => {
            50
        },
        WhiteBishop | BlackBishop => {
            30
        },
        WhiteUnicorn | BlackUnicorn => {
            70
        },
        WhiteBalloon | BlackBalloon => {
            30
        },
        WhiteRook | BlackRook => {
            30
        },
        WhiteQueen | BlackQueen => {
            120
        },
        WhiteKing | BlackKing => {
            900
        },
    }
}

pub fn evaluate_side(board: &Board, king : Turn) -> i32 {
    match king {
        Turn::White => {
            Piece::white_iterator().fold(0, |acc, &p| {
                acc + piece_strength(p) * board.get_set(p).unwrap_or(&HashSet::new()).len() as i32
            })
        },
        Turn::Black => {
            Piece::black_iterator().fold(0, |acc, &p| {
                acc + piece_strength(p) * board.get_set(p).unwrap_or(&HashSet::new()).len() as i32
            })
        },
    }
}

pub fn evaluate_state(board: &Board) -> i32 {
    evaluate_side(board, Turn::White) - evaluate_side(board, Turn::Black)
}

#[wasm_bindgen]
pub fn play_best_move(board: &mut Board, init_depth : i32, maximizing_player: bool) -> bool {
    let m = minimax_recursion(board, init_depth, std::i32::MIN, std::i32::MAX, maximizing_player).0;
    if let Some(m) = m {
        board.make_move(m);
        false
    } else {
        true
    }
}

#[derive(Debug)]
pub struct MinimaxMove (Option<Move>, i32);

pub fn minimax_recursion(board: &mut Board, depth: i32, mut alpha: i32, mut beta: i32, maximizing_player: bool) -> MinimaxMove {
    if maximizing_player {
        board.set_white_moves();
    } else {
        board.set_black_moves();
    }
    let moves = board.get_possible_moves();
    if depth == 0 {
        return MinimaxMove(None, evaluate_state(board));
    }
    if moves.is_empty() {
        if maximizing_player {
            return MinimaxMove(None, std::i32::MIN);
        } else {
            return MinimaxMove(None, std::i32::MAX);
        }
    }
    let mut value = if maximizing_player { std::i32::MIN } else { std::i32::MAX };
    let mut best_move = None;
    if maximizing_player {
        for m in moves {
            board.make_move(m);
            let mm = minimax_recursion(board, depth - 1, alpha, beta, false);
            if mm.1 > value {
                value = mm.1;
                best_move = Some(m);
            }
            alpha = alpha.max(value);
            board.undo_move();
            if alpha >= beta {
                break;
            }

        }
    } else {
        for m in moves {
            board.make_move(m);
            let mm = minimax_recursion(board, depth - 1, alpha, beta, true);
            if mm.1 < value {
                value = mm.1;
                best_move = Some(m);
            }
            beta = beta.min(value);
            board.undo_move();
            if alpha >= beta {
                break;
            }

        }
    }

    MinimaxMove(best_move, value)
}