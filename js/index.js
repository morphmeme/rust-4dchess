import {Board, play_best_move} from "../pkg";


const PIECE = ["WhitePawn" ,
    "WhiteKnight",
    "WhiteBishop",
    "WhiteUnicorn",
    "WhiteBalloon",
    "WhiteRook",
    "WhiteQueen",
    "WhiteKing",
    "BlackPawn",
    "BlackKnight",
    "BlackBishop",
    "BlackUnicorn",
    "BlackBalloon",
    "BlackRook",
    "BlackQueen",
    "BlackKing"];

let board = Board.init_board();
let table = [];

let img_create = (src) => {
    var img = document.createElement('img');
    img.src = src;
    return img;
};

let update_area = (rows, cols) => {
    console.log("test");
    for (let r=0; r<rows; ++r){
        for (let c=0; c<cols; ++c){
            table[r][c].removeChild(table[r][c].firstChild);
            table[r][c].appendChild(img_create("images/" + PIECE[board.get_piece_2d(c,r)] + ".png"));
        }
    }
};

let clickable_grid = ( rows, cols, callback ) => {
    let i = 0;
    let grid = document.createElement('table');
    grid.className = 'grid';
    for (let r=0; r<rows; ++r){
        let tr = grid.appendChild(document.createElement('tr'));
        let row = [];
        for (let c=0; c<cols; ++c){
            let cell = tr.appendChild(document.createElement('td'));
            if (r % 4 === 3) {
                $(cell).addClass("bottom");
            }
            if (c % 4 === 3) {
                $(cell).addClass("right");
            }
            if ((r + c) % 2 === 0) {
                cell.style = "background-color: #B58863";
            } else {
                cell.style = "background-color: #F0D9B5";
            }
            row.push(cell);
            cell.appendChild(img_create("images/" + PIECE[board.get_piece_2d(c,r)] + ".png"));
            cell.addEventListener('click',(function(el,r,c,i){
                return function(){
                    callback(el,r,c,i);
                }
            })(cell,r,c,i),false);
        }
        table.push(row);
    }
    return grid;
}


// function KeyPress(e) {
//     var evtobj = window.event? event : e
//     if (evtobj.keyCode == 90 && evtobj.ctrlKey) {
//         board.undo_move();
//         update_area(rows, cols);
//     }
// }
//
// document.onkeydown = KeyPress;
board.set_white_moves();

const rows = 16;
const cols = 16;
let last_clicked;
let last_row;
let last_col;
let turn = 1;
let grid = clickable_grid(rows,cols, function(el,row,col,i){
    if (last_clicked) $(last_clicked).removeClass("clicked");
    if ($(table[row][col]).hasClass("possible")) {
        board.move_piece_2d(last_col, last_row, col, row);
        update_area(rows, cols);
        setTimeout(function () {
            let game_over = play_best_move(board, 1, false);
            if (game_over) {
                alert("You win!")
            }
            update_area(rows, cols);
            // turn *= -1;
            if (turn === -1) {
                board.set_black_moves();
            } else {
                board.set_white_moves();
            }
        }, 10);

    }

    $(".possible").removeClass("possible");

    let json_str = board.get_moves();
    let moves_json = JSON.parse(json_str);
    let moves = moves_json[`[${col}, ${row}]`];

    if (!moves_json) {
        alert("Game over. You lose.")
    }

    if (moves) {
        for (let move of moves) {
            $(table[move[1]][move[0]]).addClass("possible")
        }
    }



    $(el).addClass('clicked');
    last_clicked = el;
    last_row = row;
    last_col = col;


});

document.body.appendChild(grid);

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function ai_vs_ai() {
    while (true) {
        play_best_move(board, 1, true);
        update_area(rows, cols);
        await sleep(100);
        play_best_move(board, 1, false);
        update_area(rows, cols)
        await sleep(100);
    }
}

// ai_vs_ai()