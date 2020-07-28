use std::collections::{HashMap, HashSet};
use std::slice::Iter;
use self::Piece::*;
use std::ops::{Index, IndexMut};
use itertools::Itertools;
use itertools::iproduct;
use wasm_bindgen::prelude::*;

pub fn negate_slice(slice: &[i32]) -> Vec<i32> {
    let mut copy = Vec::from(slice);
    for i in 0..copy.len() {
        copy[i] = -slice[i];
    }
    copy
}

pub fn clone_vec<T: Copy>(vec: Vec<&T>) -> Vec<T> {
    vec.into_iter().copied().collect()
}

#[wasm_bindgen]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Piece {
    WhitePawn   ,
    WhiteKnight,
    WhiteBishop,
    WhiteUnicorn,
    WhiteBalloon,
    WhiteRook,
    WhiteQueen,
    WhiteKing,
    BlackPawn,
    BlackKnight,
    BlackBishop,
    BlackUnicorn,
    BlackBalloon,
    BlackRook,
    BlackQueen,
    BlackKing,
}

impl Piece{
    pub fn iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 16] = [WhitePawn,WhiteKnight,WhiteBishop,WhiteUnicorn, WhiteBalloon, WhiteRook, WhiteQueen, WhiteKing,
            BlackPawn, BlackKnight, BlackBishop, BlackUnicorn, BlackBalloon, BlackRook, BlackQueen, BlackKing];
        PIECES.iter()
    }

    pub fn white_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 8] = [WhitePawn,WhiteKnight,WhiteBishop,WhiteUnicorn, WhiteBalloon, WhiteRook, WhiteQueen, WhiteKing];
        PIECES.iter()
    }

    pub fn white_promotion() -> Iter<'static, Piece> {
        static PIECES : [Piece; 7] = [WhiteKnight,WhiteBishop,WhiteUnicorn, WhiteBalloon, WhiteRook, WhiteQueen, WhiteKing];
        PIECES.iter()
    }

    pub fn black_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 8] = [BlackPawn, BlackKnight, BlackBishop, BlackUnicorn, BlackBalloon, BlackRook, BlackQueen, BlackKing];
        PIECES.iter()
    }

    pub fn black_promotion() -> Iter<'static, Piece> {
        static PIECES : [Piece; 7] = [BlackKnight, BlackBishop, BlackUnicorn, BlackBalloon, BlackRook, BlackQueen, BlackKing];
        PIECES.iter()
    }

    pub fn is_white(&self) -> bool {
        match self {
            WhitePawn | WhiteKnight | WhiteBishop | WhiteUnicorn | WhiteBalloon | WhiteRook | WhiteQueen | WhiteKing  => true,
            _ => false
        }
    }

    pub fn is_black(&self) -> bool {
        !self.is_white()
    }

    pub fn is_slider(&self) -> bool {
        match self {
            BlackPawn | BlackKnight | BlackKing | WhitePawn | WhiteKnight | WhiteKing => false,
            _ => true
        }
    }

    pub fn white_slider_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 5] = [WhiteBishop,WhiteUnicorn, WhiteBalloon, WhiteRook, WhiteQueen];
        PIECES.iter()
    }

    pub fn black_slider_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 5] = [BlackBishop,BlackUnicorn, BlackBalloon, BlackRook, BlackQueen];
        PIECES.iter()
    }

    pub fn white_2ddiag_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 2] = [WhiteBishop, WhiteQueen];
        PIECES.iter()
    }

    pub fn white_3ddiag_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 2] = [WhiteUnicorn, WhiteQueen];
        PIECES.iter()
    }

    pub fn white_4ddiag_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 2] = [WhiteBalloon, WhiteQueen];
        PIECES.iter()
    }

    pub fn black_2ddiag_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 2] = [BlackBishop, BlackQueen];
        PIECES.iter()
    }

    pub fn black_3ddiag_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 2] = [BlackUnicorn, BlackQueen];
        PIECES.iter()
    }

    pub fn black_4ddiag_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 2] = [BlackBalloon, BlackQueen];
        PIECES.iter()
    }

    pub fn white_straight_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 2] = [WhiteRook, WhiteQueen];
        PIECES.iter()
    }

    pub fn black_straight_iterator() -> Iter<'static, Piece> {
        static PIECES : [Piece; 2] = [BlackRook, BlackQueen];
        PIECES.iter()
    }
}

const DIM: usize = 4;
#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub struct Position([usize; DIM]);

impl Position {
    pub fn new(slice: [usize; DIM]) -> Self {
        Position(slice)
    }
    pub fn iter(&self) -> Iter<usize> {
        self.0.iter()
    }

    pub fn add_at(&self, dims: &[usize], vals: &[i32]) -> Option<Self> {
        let mut res = self.clone();
        for i in 0..dims.len() {
            let sum = res[dims[i]] as i32 + vals[i];
            // bounds check
            if sum < 0 || sum >= DIM as i32 {
                return None;
            }
            res[dims[i]] = sum as usize;
        }
        Some(res)
    }

    pub fn project(&self) -> [usize;2] {
        /*
        n = vec.i + vec.j * 4 + vec.k * 16 + vec.l * 64
        n_matrices = self.j * self.l
        x = n % n_matrices
        y = n // n_matrices
        return x, y
        */
        let n = self.0[0] + self.0[1] * 4 + self.0[2] * 16 + self.0[3] * 64;
        let nb_matrices = DIM * DIM;
        let x = n % nb_matrices;
        let y = n / nb_matrices;
        [x, y]
    }

    pub fn from_2d(x: usize, y: usize) -> Self {
        Position::new([x % 4, x / 4, y % 4, y / 4])
    }
}

impl Index<usize> for Position {
    type Output = usize;

    fn index(&self, index: usize) -> &Self::Output {
        self.0.index(index)
    }
}

impl IndexMut<usize> for Position {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.0.index_mut(index)
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub struct Move {
    pub from : Position,
    pub to : Position,
    pub upgrade : Option<Piece>
}

#[wasm_bindgen]
#[derive(Debug)]
pub struct Board {
    // board representation
    pieces: HashMap<Position, Piece>,
    piece_sets: HashMap<Piece, HashSet<Position>>,
    possible_moves: HashMap<Position, HashSet<Position>>,
    capture_mask: HashSet<Position>,
    push_mask: HashSet<Position>,
    pin_masks: HashMap<Position, HashSet<Position>>,
    last_piece_moved : Vec<(Piece, Position, Position)>,
    last_piece_eaten : Vec<Option<(Piece, Position)>>
}

// attack sets
impl Board {
    fn clear_moves(&mut self) {
        self.possible_moves.clear();
        self.capture_mask = Board::all_positions();
        self.push_mask = self.capture_mask.clone();
        self.pin_masks = HashMap::new();
    }

    fn knight_attack_set(&self, pos: Position) -> HashSet<Position> {
        let mut set = HashSet::new();
        let directions = [[1,2], [2,1], [-1,-2], [-2,-1], [-1,2], [-2,1], [1,-2], [2,-1]];
        for x in 0..4 {
            for y in x+1..4 {
                for dir in directions.iter() {
                    let possible_move = pos.add_at(&[x,y], dir);
                    if let Some(move_) = possible_move {
                        set.insert(move_);
                    }
                }
            }
        }
        set
    }

    fn pawn_attack_set(&self, piece: Piece, pos: Position) -> HashSet<Position> {
        let deltas = if piece.is_white() {
            [[1, -1], [-1, -1]]
        } else {
            [[-1, 1], [1, 1]]
        };
        let mut set = HashSet::new();
        let dims = [[0,2], [0,3], [1,2], [1,3]];
        for (dim, delta) in iproduct!(dims.iter(), deltas.iter()) {
            let potential_move = pos.add_at(dim, delta);
            if let Some(move_) = potential_move {
                set.insert(move_);
            }
        }
        set
    }

    fn slide_set(&self, piece: Piece, pos: Position, dims: &[usize], direction: &[i32]) -> (HashSet<Position>, bool) {
        let mut set = HashSet::new();
        let mut some_next = pos.add_at(dims, direction);
        let mut path_has_king = false;
        while let Some(next) = some_next {
            set.insert(next);
            if self.is_occupied(&next) {
                if self.piece_sets.get(&piece).unwrap().contains(&next) {
                    path_has_king = true;
                }
                break;
            }
            some_next = next.add_at(dims, direction);
        }
        (set, path_has_king)
    }

    pub fn fourd_diagonal_directions() -> &'static [&'static[i32]]{
        &[&[-1, -1, -1, -1],&[-1, -1, -1, 1],&[-1, -1, 1, -1],&[-1, -1, 1, 1],&[-1, 1, -1, -1],&[-1, 1, -1, 1],&[-1, 1, 1, -1],&[-1, 1, 1, 1],&[1, -1, -1, -1],&[1, -1, -1, 1],&[1, -1, 1, -1],&[1, -1, 1, 1],&[1, 1, -1, -1],&[1, 1, -1, 1],&[1, 1, 1, -1],&[1, 1, 1, 1]]
    }

    pub fn threed_diagonal_directions() -> &'static[&'static[i32]] {
        &[&[-1, -1, -1, 0],&[-1, -1, 1, 0],&[-1, 1, -1, 0],&[-1, 1, 1, 0],&[1, -1, -1, 0],&[1, -1, 1, 0],&[1, 1, -1, 0],&[1, 1, 1, 0],&[-1, -1, 0, -1],&[-1, -1, 0, 1],&[-1, 1, 0, -1],&[-1, 1, 0, 1],&[1, -1, 0, -1],&[1, -1, 0, 1],&[1, 1, 0, -1],&[1, 1, 0, 1],&[-1, 0, -1, -1],&[-1, 0, -1, 1],&[-1, 0, 1, -1],&[-1, 0, 1, 1],&[1, 0, -1, -1],&[1, 0, -1, 1],&[1, 0, 1, -1],&[1, 0, 1, 1],&[0, -1, -1, -1],&[0, -1, -1, 1],&[0, -1, 1, -1],&[0, -1, 1, 1],&[0, 1, -1, -1],&[0, 1, -1, 1],&[0, 1, 1, -1],&[0, 1, 1, 1]]
    }

    pub fn twod_diagonal_directions() -> &'static[&'static[i32]] {
        &[&[-1, -1, 0, 0],&[-1, 1, 0, 0],&[1, -1, 0, 0],&[1, 1, 0, 0],&[-1, 0, -1, 0],&[-1, 0, 1, 0],&[1, 0, -1, 0],&[1, 0, 1, 0],&[-1, 0, 0, -1],&[-1, 0, 0, 1],&[1, 0, 0, -1],&[1, 0, 0, 1],&[0, -1, -1, 0],&[0, -1, 1, 0],&[0, 1, -1, 0],&[0, 1, 1, 0],&[0, -1, 0, -1],&[0, -1, 0, 1],&[0, 1, 0, -1],&[0, 1, 0, 1],&[0, 0, -1, -1],&[0, 0, -1, 1],&[0, 0, 1, -1],&[0, 0, 1, 1]]
    }

    pub fn diagonal_directions(nb_dims: usize) -> &'static[&'static[i32]] {
        match nb_dims {
            4 => Board::fourd_diagonal_directions(),
            3 => Board::threed_diagonal_directions(),
            2 => Board::twod_diagonal_directions(),
            _ => unreachable!()
        }
    }

    fn diagonal_attack_set(&self, piece: Piece, nb_dims: usize, pos: Position) -> (HashSet<Position>, Option<HashSet<Position>>) {
        let mut set = HashSet::new();
        let mut path_with_king = None;
        for dir in Board::diagonal_directions(nb_dims) {
            // TODO why clone?
            let path = self.slide_set(piece, pos, &[0,1,2,3], dir);
            if path.1 {
                path_with_king = Some(path.0.clone());
            }
            set.extend(path.0);
        }
        (set, path_with_king)
    }

    fn bishop_attack_set(&self, piece: Piece, pos: Position) -> (HashSet<Position>, Option<HashSet<Position>>) {
        self.diagonal_attack_set(piece, 2, pos)
    }

    fn unicorn_attack_set(&self, piece: Piece, pos: Position) -> (HashSet<Position>, Option<HashSet<Position>>) {
        self.diagonal_attack_set(piece, 3, pos)
    }

    fn balloon_attack_set(&self, piece: Piece, pos: Position) -> (HashSet<Position>, Option<HashSet<Position>>) {
        self.diagonal_attack_set(piece, 4, pos)
    }

    pub fn straight_directions() -> &'static[&'static[i32]] {
        &[&[1, 0, 0, 0], &[0, 1, 0, 0], &[0, 0, 1, 0], &[0, 0, 0, 1],
            &[-1, 0, 0, 0], &[0, -1, 0, 0], &[0, 0, -1, 0], &[0, 0, 0, -1]]
    }

    fn rook_attack_set(&self, piece: Piece, pos: Position) -> (HashSet<Position>, Option<HashSet<Position>>) {
        let mut set = HashSet::new();
        let mut path_with_king = None;
        for dir in Board::straight_directions() {
            let path = self.slide_set(piece, pos, &[0,1,2,3], dir);
            if path.1 {
                path_with_king = Some(path.0.clone());
            }
            set.extend(path.0);
        }
        (set, path_with_king)
    }

    fn queen_attack_set(&self, piece: Piece, pos: Position) -> (HashSet<Position>, Option<HashSet<Position>>) {
        let mut bishop_set = self.bishop_attack_set(piece, pos);
        let unicorn_set = self.unicorn_attack_set(piece, pos);
        let balloon_set = self.balloon_attack_set(piece, pos);
        let rook_set = self.rook_attack_set(piece, pos);
        bishop_set.0.extend(unicorn_set.0);
        bishop_set.0.extend(balloon_set.0);
        bishop_set.0.extend(rook_set.0);

        (bishop_set.0, bishop_set.1.or(unicorn_set.1).or(balloon_set.1).or(rook_set.1))
    }

    fn neighbors(&self, dim: usize, pos: Position) -> HashSet<Position> {
        let mut set = HashSet::new();
        let dims_sets = [0,1,2,3].iter().combinations(dim);
        let dirs_sets = (0..dim).map(|_| [0, 1, -1].iter()).multi_cartesian_product();
        for (dims, dir) in iproduct!(dims_sets, dirs_sets) {
            let some_pos = pos.add_at(clone_vec(dims).as_slice(), clone_vec(dir).as_slice());
            if let Some(pos) = some_pos {
                set.insert(pos);
            }
        }
        set
    }

    fn king_attack_set(&self, pos: Position) -> HashSet<Position> {
        let mut set = HashSet::new();
        set.extend(self.neighbors(4, pos));
        set.extend(self.neighbors(3, pos));
        set.extend(self.neighbors(2, pos));
        set.remove(&pos);
        set
    }

    pub fn attack_sets(&self, piece: Piece, pos: Position) -> (HashSet<Position>, Option<HashSet<Position>>) {
        match piece {
            WhitePawn | BlackPawn => {
                (self.pawn_attack_set(piece, pos), None)
            },
            WhiteKnight | BlackKnight => {
                (self.knight_attack_set(pos), None)
            },
            WhiteBishop | BlackBishop => {
                self.bishop_attack_set(piece, pos)
            },
            WhiteUnicorn | BlackUnicorn => {
                self.unicorn_attack_set(piece, pos)
            },
            WhiteBalloon | BlackBalloon => {
                self.balloon_attack_set(piece, pos)
            },
            WhiteRook | BlackRook => {
                self.rook_attack_set(piece, pos)
            },
            WhiteQueen | BlackQueen => {
                self.queen_attack_set(piece, pos)
            },
            WhiteKing | BlackKing => {
                (self.king_attack_set(pos), None)
            },
        }
    }
    pub fn pawn_move_set(&self, piece: Piece, pos: Position) -> HashSet<Position> {
        let mut set = HashSet::new();
        if piece.is_white() {
            if let Some(new_pos) = pos.add_at(&[0,1,2,3], &[0,0,-1,0]) {
                if !self.pieces.contains_key(&new_pos) {
                    set.insert(new_pos);
                }
            }
            if let Some(new_pos) = pos.add_at(&[0,1,2,3], &[0,1,0,0]) {
                if !self.pieces.contains_key(&new_pos) {
                    set.insert(new_pos);
                }
            }
        } else {
            if let Some(new_pos) = pos.add_at(&[0,1,2,3], &[0,0,1,0]) {
                if !self.pieces.contains_key(&new_pos) {
                    set.insert(new_pos);
                }
            }
            if let Some(new_pos) = pos.add_at(&[0,1,2,3], &[0,-1,0,0]) {
                if !self.pieces.contains_key(&new_pos) {
                    set.insert(new_pos);
                }
            }
        };
        set
    }

    fn filter_white_pawn_attacks(&self, pawn_attacks: &mut HashSet<Position>, black_pieces: &HashSet<Position>)  {
        pawn_attacks.retain(|pos| black_pieces.contains(pos));
    }

    fn filter_black_pawn_attacks(&self, pawn_attacks: &mut HashSet<Position>, white_pieces: &HashSet<Position>)  {
        pawn_attacks.retain(|pos| white_pieces.contains(pos));
    }
}

// moves
impl Board {
    fn all_positions() -> HashSet<Position> {
        let mut positions = HashSet::new();
        for (i, j, k, l) in iproduct!(0..DIM, 0..DIM, 0..DIM, 0..DIM) {
            positions.insert(Position([i,j,k,l]));
        }
        positions
    }

    fn set_king_moves(&mut self, king: Piece, friendly_pieces: &HashSet<Position>) {
        let king_pos = self.king_pos(king);
        self.remove(king_pos);
        let mut attacked_pos = HashSet::new();
        if king.is_white() {
            for piece in Piece::black_iterator() {
                for positions in self.piece_sets.get(piece) {
                    for position in positions {
                        let set = self.attack_sets(*piece, *position);
                        attacked_pos.extend(set.0);
                    }
                }
            }
        } else {
            for piece in Piece::white_iterator() {
                for positions in self.piece_sets.get(piece) {
                    for position in positions {
                        let set = self.attack_sets(*piece, *position);
                        attacked_pos.extend(set.0);
                    }
                }
            }
        }
        self.set(king_pos, king);
        let mut initial_set = self.king_attack_set(king_pos);
        initial_set.retain(|pos| !friendly_pieces.contains(pos));
        self.possible_moves.insert(king_pos, initial_set.difference(&attacked_pos).cloned().collect());
    }

    // first param are the attackers, second param is the path for single check
    fn king_attackers(&self, king: Piece) -> (HashSet<Position>, Option<HashSet<Position>>) {
        let mut king_attackers_set = HashSet::new();
        let mut push_mask = None;
        if king.is_white() {
            let king_pos = self.white_king_pos();
            for piece in Piece::black_iterator() {
                let set = self.piece_sets.get(piece).unwrap();
                let attackers = if piece == &BlackPawn {
                    self.attack_sets(WhitePawn, king_pos)
                } else {
                    self.attack_sets(*piece, king_pos)
                };
                king_attackers_set.extend(attackers.0.intersection(set));
                if attackers.1.is_some() {
                    push_mask = attackers.1;
                }
            }
        } else {
            let king_pos = self.black_king_pos();
            for piece in Piece::white_iterator() {
                let set = self.piece_sets.get(piece).unwrap();
                let attackers = if piece == &WhitePawn {
                    self.attack_sets(BlackPawn, king_pos)
                } else {
                    self.attack_sets(*piece, king_pos)
                };
                king_attackers_set.extend(attackers.0.intersection(set));
                if attackers.1.is_some() {
                    push_mask = attackers.1;
                }
            }
        }
        (king_attackers_set, push_mask)
    }


    fn set_pin_mask(&mut self, piece: &Piece, dir: &[i32], king_pos: Position)  {
        for set in self.piece_sets.get(piece) {
            for &pos in set {
                let enemy_slide = self.slide_set(*piece, pos, &[0,1,2,3], dir);
                let king_slide = self.slide_set(WhiteKing, king_pos, &[0,1,2,3], negate_slice(dir).as_slice());
                let pin_ray: HashSet<_> = enemy_slide.0.intersection(&king_slide.0).collect();
                for some_pin_pos in pin_ray {
                    if self.pieces.contains_key(some_pin_pos) {
                        let mut path : HashSet<_> = enemy_slide.0.union(&king_slide.0).copied().collect();
                        path.insert(pos);
                        self.pin_masks.insert(*some_pin_pos, path);
                    }
                }
            }
        }
    }


    fn set_white_pin_masks(&mut self) {
        let king_pos = self.white_king_pos();
        // for all directions
        for dir in Board::twod_diagonal_directions() {
            for piece in Piece::black_2ddiag_iterator() {
                self.set_pin_mask(piece, dir, king_pos)
            }
        }
        for dir in Board::threed_diagonal_directions() {
            for piece in Piece::black_3ddiag_iterator() {
                self.set_pin_mask(piece, dir, king_pos)
            }
        }
        for dir in Board::fourd_diagonal_directions() {
            for piece in Piece::black_4ddiag_iterator() {
                self.set_pin_mask(piece, dir, king_pos)
            }
        }
        for dir in Board::straight_directions() {
            for piece in Piece::black_straight_iterator() {
                self.set_pin_mask(piece, dir, king_pos)
            }
        }
    }

    fn set_black_pin_masks(&mut self) {
        let king_pos = self.black_king_pos();
        // for all directions
        for dir in Board::twod_diagonal_directions() {
            for piece in Piece::white_2ddiag_iterator() {
                self.set_pin_mask(piece, dir, king_pos)
            }
        }
        for dir in Board::threed_diagonal_directions() {
            for piece in Piece::white_3ddiag_iterator() {
                self.set_pin_mask(piece, dir, king_pos)
            }
        }
        for dir in Board::fourd_diagonal_directions() {
            for piece in Piece::white_4ddiag_iterator() {
                self.set_pin_mask(piece, dir, king_pos)
            }
        }
        for dir in Board::straight_directions() {
            for piece in Piece::white_straight_iterator() {
                self.set_pin_mask(piece, dir, king_pos)
            }
        }
    }
}

#[wasm_bindgen]
impl Board {
    pub fn init_board() -> Self {
        let mut board = Self::new();
        board.set(Position::new([0,0,0,0]), BlackKnight);
        board.set(Position::new([1,0,0,0]), BlackBishop);
        board.set(Position::new([2,0,0,0]), BlackBishop);
        board.set(Position::new([3,0,0,0]), BlackKnight);
        board.set(Position::new([0,1,0,0]), BlackKnight);
        board.set(Position::new([1,1,0,0]), BlackUnicorn);
        board.set(Position::new([2,1,0,0]), BlackUnicorn);
        board.set(Position::new([3,1,0,0]), BlackKnight);
        board.set(Position::new([0,2,0,0]), BlackRook);
        board.set(Position::new([1,2,0,0]), BlackBalloon);
        board.set(Position::new([2,2,0,0]), BlackBalloon);
        board.set(Position::new([3,2,0,0]), BlackRook);
        board.set(Position::new([0,3,0,0]), BlackRook);
        board.set(Position::new([1,3,0,0]), BlackBalloon);
        board.set(Position::new([2,3,0,0]), BlackBalloon);
        board.set(Position::new([3,3,0,0]), BlackKing);
        board.set(Position::new([0,2,0,1]), BlackUnicorn);
        board.set(Position::new([1,2,0,1]), BlackBalloon);
        board.set(Position::new([2,2,0,1]), BlackBalloon);
        board.set(Position::new([3,2,0,1]), BlackUnicorn);
        board.set(Position::new([0,3,0,1]), BlackRook);
        board.set(Position::new([1,3,0,1]), BlackBalloon);
        board.set(Position::new([2,3,0,1]), BlackBalloon);
        board.set(Position::new([3,3,0,1]), BlackQueen);
        for l in 0..2 {
            for j in 0..DIM {
                for i in 0..DIM {
                    if l == 1 && j < 2 {
                        continue;
                    }
                    board.set(Position::new([i,j,1,l]), BlackPawn);
                }
            }
        }

        for l in 2..4 {
            for j in 0..DIM {
                for i in 0..DIM {
                    if l == 2 && j >= 2 {
                        continue;
                    }
                    board.set(Position::new([i,j,2,l]), WhitePawn);
                }
            }
        }

        board.set(Position::new([3,3,3,3]), WhiteKnight);
        board.set(Position::new([2,3,3,3]), WhiteBishop);
        board.set(Position::new([1,3,3,3]), WhiteBishop);
        board.set(Position::new([0,3,3,3]), WhiteKnight);
        board.set(Position::new([3,2,3,3]), WhiteKnight);
        board.set(Position::new([2,2,3,3]), WhiteUnicorn);
        board.set(Position::new([1,2,3,3]), WhiteUnicorn);
        board.set(Position::new([0,2,3,3]), WhiteKnight);
        board.set(Position::new([3,1,3,3]), WhiteRook);
        board.set(Position::new([2,1,3,3]), WhiteBalloon);
        board.set(Position::new([1,1,3,3]), WhiteBalloon);
        board.set(Position::new([0,1,3,3]), WhiteRook);
        board.set(Position::new([3,0,3,3]), WhiteRook);
        board.set(Position::new([2,0,3,3]), WhiteBalloon);
        board.set(Position::new([1,0,3,3]), WhiteBalloon);
        board.set(Position::new([0,0,3,3]), WhiteKing);
        board.set(Position::new([3,1,3,2]), WhiteUnicorn);
        board.set(Position::new([2,1,3,2]), WhiteBalloon);
        board.set(Position::new([1,1,3,2]), WhiteBalloon);
        board.set(Position::new([0,1,3,2]), WhiteUnicorn);
        board.set(Position::new([3,0,3,2]), WhiteRook);
        board.set(Position::new([2,0,3,2]), WhiteBalloon);
        board.set(Position::new([1,0,3,2]), WhiteBalloon);
        board.set(Position::new([0,0,3,2]), WhiteQueen);
        board
    }

    pub fn set_white_moves(&mut self) {
        self.clear_moves();
        let attackers = self.king_attackers(WhiteKing);
        let white_pieces = self.white_pieces();
        let black_pieces = self.black_pieces();
        self.set_king_moves(WhiteKing, &white_pieces);

        match attackers.0.len() {
            0 => {
                // not in check.
            },
            1 => {
                self.capture_mask = attackers.0;
                if let Some(push_mask) = attackers.1 {
                    self.push_mask = push_mask;
                } else {
                    self.push_mask = HashSet::new();
                }
            },
            _ => {
                // double check -> only king moves
                return;
            }
        }
        self.set_white_pin_masks();
        let filter: HashSet<_> = self.capture_mask.union(&self.push_mask).copied().collect();

        for piece in Piece::white_iterator() {
            for positions in self.piece_sets.get(piece) {
                for pos in positions {
                    if self.possible_moves.contains_key(pos) {
                        continue;
                    }
                    let mut attack_set = self.attack_sets(*piece, *pos);
                    if *piece == WhitePawn {
                        self.filter_white_pawn_attacks(&mut attack_set.0, &black_pieces);
                        attack_set.0.extend(self.pawn_move_set(*piece, *pos));
                    }
                    let mut filtered_set : HashSet<_> = attack_set.0.intersection(&filter).copied().collect();
                    filtered_set.retain(|pos| !white_pieces.contains(pos));
                    let pin_mask = self.pin_masks.get(pos);
                    if let Some(pin_mask) = pin_mask {
                        filtered_set = filtered_set.intersection(pin_mask).copied().collect();
                    }
                    self.possible_moves.insert(*pos, filtered_set);
                }
            }
        }
    }

    pub fn set_black_moves(&mut self) {
        self.clear_moves();
        let attackers = self.king_attackers(BlackKing);
        let white_pieces = self.white_pieces();
        let black_pieces = self.black_pieces();
        self.set_king_moves(BlackKing, &black_pieces);


        match attackers.0.len() {
            0 => {
                // not in check.
            },
            1 => {
                self.capture_mask = attackers.0;
                if let Some(push_mask) = attackers.1 {
                    self.push_mask = push_mask;
                } else {
                    self.push_mask = HashSet::new();
                }
            },
            _ => {
                // double check -> only king moves
                return;
            }
        }
        self.set_black_pin_masks();
        let filter: HashSet<_> = self.capture_mask.union(&self.push_mask).copied().collect();
        for piece in Piece::black_iterator() {
            for positions in self.piece_sets.get(piece) {
                for pos in positions {
                    if self.possible_moves.contains_key(pos) {
                        continue;
                    }
                    let mut attack_set = self.attack_sets(*piece, *pos);
                    if *piece == BlackPawn {
                        self.filter_black_pawn_attacks(&mut attack_set.0, &white_pieces);
                        attack_set.0.extend(self.pawn_move_set(*piece, *pos));
                    }
                    let mut filtered_set : HashSet<_> = attack_set.0.intersection(&filter).copied().collect();
                    filtered_set.retain(|pos| !black_pieces.contains(pos));
                    let pin_mask = self.pin_masks.get(pos);
                    if let Some(pin_mask) = pin_mask {
                        filtered_set = filtered_set.intersection(pin_mask).copied().collect();
                    }
                    self.possible_moves.insert(*pos, filtered_set);
                }
            }
        }
    }

    pub fn get_positions(&self) -> String {
        let mut json = String::from("{");
        for (pos, piece) in self.pieces.iter() {
            json.push_str(format!("\"{:?}\": \"{:?}\",", pos.project(), piece).as_str());
        }
        json = json.trim_end_matches(",").to_owned();
        json.push('}');
        json
    }

    pub fn get_moves(&self) -> String {
        let mut json = String::from("{");
        for (j, (from_pos, to_positions)) in self.possible_moves.iter().enumerate() {
            json.push_str(format!("\"{:?}\": [", from_pos.project()).as_str());
            for (i, to_pos) in to_positions.iter().enumerate() {
                if i == to_positions.len() - 1 {
                    json.push_str(format!("{:?}", to_pos.project()).as_str());
                } else {
                    json.push_str(format!("{:?},", to_pos.project()).as_str());
                }
            }
            if j == self.possible_moves.len() - 1 {
                json.push_str("]");
            } else {
                json.push_str("],");
            }
        }
        json.push('}');
        json
    }

    pub fn move_piece_2d(&mut self, x1: usize, y1: usize, x2: usize, y2: usize, upgrade : Option<Piece>) {
        let from = Position::from_2d(x1, y1);
        let to = Position::from_2d(x2, y2);
        self.move_piece(from, to, upgrade);
    }

    pub fn undo_move(&mut self) {
        if let Some((piece, from, to)) = self.last_piece_moved.pop() {
            self.remove(to);
            self.set(from, piece);
        }
        if let Some(Some((piece,pos))) = self.last_piece_eaten.pop() {
            self.set(pos, piece);
        }
    }

    pub fn get_piece_2d(&self, x: usize, y: usize) -> Option<Piece> {
        self.get(Position::from_2d(x, y)).map(|x| *x)
    }
}

impl Board {
    pub fn new() -> Self {
        let mut piece_sets = HashMap::new();
        for &piece in Piece::iterator() {
            piece_sets.insert(piece, HashSet::new());
        }
        let capture_mask = Board::all_positions();
        let push_mask = capture_mask.clone();
        Self {
            pieces: HashMap::new(),
            piece_sets,
            possible_moves: HashMap::new(),
            capture_mask,
            push_mask,
            pin_masks: HashMap::new(),
            last_piece_moved: vec![],
            last_piece_eaten: vec![]
        }
    }

    pub fn get(&self, pos: Position) -> Option<&Piece> {
        self.pieces.get(&pos)
    }

    pub fn get_set(&self, piece: Piece) -> Option<&HashSet<Position>> {
        self.piece_sets.get(&piece)
    }

    pub fn get_mut_set(&mut self, piece: Piece) -> Option<&mut HashSet<Position>> {
        self.piece_sets.get_mut(&piece)
    }

    pub fn set(&mut self, pos: Position, piece: Piece) -> Option<Piece> {
        let replaced_piece = self.remove(pos);
        self.pieces.insert(pos, piece);

        let piece_set = self.get_mut_set(piece);
        if let Some(piece_set) = piece_set {
            piece_set.insert(pos);
        }

        replaced_piece
    }

    pub fn remove(&mut self, pos: Position) -> Option<Piece> {
        let removed_piece = self.pieces.remove(&pos);
        if let Some(replaced_piece) = removed_piece {
            let replaced_piece_set = self.get_mut_set(replaced_piece);
            if let Some(replaced_piece_set) = replaced_piece_set {
                replaced_piece_set.remove(&pos);
            }
        }
        removed_piece
    }

    fn move_piece(&mut self, from: Position, to: Position, upgrade: Option<Piece>) -> Option<Piece> {
        let mut removed_piece = None;
        if let Some(&piece) = self.get(from) {
            self.last_piece_moved.push((piece, from, to));
            removed_piece = self.set(to, upgrade.unwrap_or(piece));
        }
        if let Some(piece) = removed_piece {
            self.last_piece_eaten.push(Some((piece, to)));
        } else {
            self.last_piece_eaten.push(None);
        }
        self.remove(from);
        removed_piece
    }

    pub fn make_move(&mut self, m : Move) {
        self.move_piece(m.from, m.to, m.upgrade);
    }

    pub fn is_occupied(&self, pos: &Position) -> bool {
        self.pieces.contains_key(pos)
    }

    fn white_king_pos(&self) -> Position {
        let king = self.piece_sets.get(&WhiteKing).unwrap();
        let mut king_pos = Position::new([0,0,0,0]);
        // Only one king
        for pos in king {
            king_pos = pos.clone();
        }
        king_pos
    }

    fn black_king_pos(&self) -> Position {
        let king = self.piece_sets.get(&BlackKing).unwrap();
        let mut king_pos = Position::new([0,0,0,0]);
        // Only one king
        for pos in king {
            king_pos = pos.clone();
        }
        king_pos
    }

    pub fn king_pos(&self, color: Piece) -> Position {
        if color.is_white() {
            self.white_king_pos()
        } else {
            self.black_king_pos()
        }
    }

    pub fn get_possible_moves(&self) -> HashSet<Move> {
        let mut moves = HashSet::new();
        for (&from_pos, to_positions) in self.possible_moves.iter() {
            let piece = *self.get(from_pos).unwrap();
            let promotion = if piece == WhitePawn &&
                (from_pos == Position::new([0, 3, 0, 0]) ||
                    from_pos == Position::new([1, 3, 0, 0]) ||
                    from_pos == Position::new([2, 3, 0, 0]) ||
                    from_pos == Position::new([3, 3, 0, 0])) {
                Some(WhitePawn)
            } else if piece == BlackPawn &&
                    (from_pos == Position::new([3, 0, 3, 3]) ||
                        from_pos == Position::new([2, 0, 3, 3]) ||
                        from_pos == Position::new([1, 0, 3, 3]) ||
                        from_pos == Position::new([0, 0, 3, 3])) {
                Some(BlackPawn)
            } else {
                None
            };
            for &to_position in to_positions {
                if let Some(promotion) = promotion {
                    if promotion == WhitePawn {
                        for &upgrade in Piece::white_promotion() {
                            moves.insert(Move {
                                from: from_pos,
                                to: to_position,
                                upgrade: Some(upgrade)
                            });
                        }
                    } else {
                        for &upgrade in Piece::black_promotion() {
                            moves.insert(Move {
                                from: from_pos,
                                to: to_position,
                                upgrade: Some(upgrade)
                            });
                        }
                    }

                } else {
                    moves.insert(Move {
                        from: from_pos,
                        to: to_position,
                        upgrade: None
                    });
                }
            }
        }
        moves
    }

    pub fn white_pieces(&self) -> HashSet<Position> {
        let mut piece_set = HashSet::new();
        for white_set in Piece::white_iterator() {
            for pieces in self.piece_sets.get(white_set) {
                for piece in pieces {
                    piece_set.insert(*piece);
                }
            }
        }
        piece_set
    }

    pub fn black_pieces(&self) -> HashSet<Position> {
        let mut piece_set = HashSet::new();
        for black_set in Piece::black_iterator() {
            for pieces in self.piece_sets.get(black_set) {
                for piece in pieces {
                    piece_set.insert(*piece);
                }
            }
        }
        piece_set
    }
}
