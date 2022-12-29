:- consult('utilities.pl').
:- consult('board.pl').

%piece(Player,X,Y)
:- dynamic piece/3.

piece(1,5,9).
piece(1,7,9).
piece(1,9,9).
piece(1,11,9).
piece(1,13,9).
piece(1,7,7).
piece(1,6,8).
piece(1,8,8).
piece(1,10,8).
piece(1,12,8).
piece(1,9,7).
piece(1,11,7).

piece(2,5,1).
piece(2,7,1).
piece(2,9,1).
piece(2,11,1).
piece(2,13,1).
piece(2,6,2).
piece(2,8,2).
piece(2,10,2).
piece(2,12,2).
piece(2,7,3).
piece(2,9,3).
piece(2,11,3).

%/----------------------------------------/

% valid_spaces(PlayerChar,SpaceChar)
valid_spaces('+','_').
valid_spaces('o','_').
valid_spaces('o','+').
valid_spaces('+','o').

%/----------------------------------------/

% valid_direction(Dir)
valid_direction('r').
valid_direction('l').

%/----------------------------------------/

%WEIGHTS: 3 - ONLY SPACE AVAILABLE // 2 - PIECE FROM OTHER PLAYER // 1 - EMPTY SPACE // 0 - NOT VALID 

% Predicates that return the pieces (piece(Player,X,Y)) in a list
list_pieces(_,_) :-
    assert(pieces([])),
    fail.

list_pieces(Player,_) :-
    piece(Player,X,Y),
    retract(pieces(L)),
    assert(pieces([piece(Player,X,Y)|L])),
    fail.

list_pieces(_,L) :-
    retract(pieces(L)).

%/----------------------------------------/

% Predicate in which the user specifies the direction he wants to move the pieces
% -Dir
get_direction(Dir) :-
    repeat,
    write('\nIn which direction do you want to move?\n'),
    write('l - Left // r - Right\n'),
    get_char(Dir),skip_line,
    valid_direction(Dir).

%/----------------------------------------/

% Predicate that checks in the database if a piece with the
% specific coordinates exists
% +[XInput,YInput] -> Coordinates from input 
% +Player
% -X
% -Y
check_existence([XInput,YInput],Player,X,Y) :-

    char_code(XLetter,XInput),
    translate_letter(XLetter,X),
    Y is YInput - 48,
    findall(Player,piece(Player,X,Y),[_|_]).

%/----------------------------------------/

% Predicates thats checks if the coordenates given are valid
% +[XInput,YInput] -> Coordinates from input 
% +Player
% -X
% -Y
check_cords([XInput,YInput],Player,X,Y) :-
    XInput >= 65,
    XInput =< 81,
    YInput >= 49,
    YInput =< 57,
    check_existence([XInput,YInput],Player,X,Y).

check_cords([XInput,YInput],Player,X,Y) :-
    XInput >= 97,
    XInput =< 113,
    YInput >= 49,
    YInput =< 57,
    check_existence([XInput,YInput],Player,X,Y).

%/----------------------------------------/

% Predicates thats checks if the coordinates given represent a valid space
% +Board -> Current Board
% +Player
% +[X,Y] -> Coordinates
check_valid_space(Board,1,[X,Y]) :-
    get_board_value(Board,Y,X,Value),
    valid_spaces('+',Value).

check_valid_space(Board,2,[X,Y]) :-
    get_board_value(Board,Y,X,Value),
    valid_spaces('o',Value).

%/----------------------------------------/

% Predicates thats checks if an attack happens at the specified coordinates and updates the database
% +Player
% +[X,Y] -> Coordinates
check_attack(1,[X,Y]) :-
    piece(2,X,Y),
    retract(piece(2,X,Y)).

check_attack(1,[X,Y]) :-
    \+piece(2,X,Y).

check_attack(2,[X,Y]) :-
    piece(1,X,Y),
    retract(piece(1,X,Y)).

check_attack(2,[X,Y]) :-
    \+piece(1,X,Y).

%/----------------------------------------/

% Predicate thats moves the chosen piece to the new space and updates the database
% +Board -> Current Board
% +Player
% +[X,Y] -> Coordinates of the chosen piece
% -NewBoard -> New Board
move_piece(Board,1,[X,Y],NewBoard) :-
    replace_board_value(Board,Y,X,'+',NewBoard),
    assert(piece(1,X,Y)).

move_piece(Board,2,[X,Y],NewBoard) :-
    replace_board_value(Board,Y,X,'o',NewBoard),
    assert(piece(2,X,Y)).

%/----------------------------------------/

% Predicate that cleans a space of the board and removes the piece from the database
% +Board -> Current Board
% +[X,Y] -> Coordinates of the space
% -NewBoard -> New Board
clean_space(Board,Player,[X,Y],NewBoard) :-
    replace_board_value(Board,Y,X,'_',NewBoard),
    retract(piece(Player,X,Y)).

%/----------------------------------------/

% Predicate that wipes a space of the board
% +Board -> Current Board
% +[X,Y] -> Coordinates of the space
% -NewBoard -> New Board
prepare_space(Board,[X,Y],NewBoard) :-
    replace_board_value(Board,Y,X,'_',NewBoard).

%/----------------------------------------/

% Predicates that get the coordinates of the new spaces where the pieces will move
get_new_space(1,[X,Y],'l',[X1,Y1]) :-
    X1 is X - 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y - 1,
    Y1 >= 1,
    Y1 =< 9.

get_new_space(1,[X,Y],'r',[X1,Y1]) :-
    X1 is X + 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y - 1,
    Y1 >= 1,
    Y1 =< 9.

get_new_space(2,[X,Y],'l',[X1,Y1]) :-
    X1 is X - 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y + 1,
    Y1 >= 1,
    Y1 =< 9.

get_new_space(2,[X,Y],'r',[X1,Y1]) :-
    X1 is X + 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y + 1,
    Y1 >= 1,
    Y1 =< 9.

%/----------------------------------------/

% Predicates that checks if the player reached the enemy's side
% +Player
check_reached_other_side(Player) :-
    \+isEven(Player),
    findall(Player,piece(Player,_,1),[_|_]).

check_reached_other_side(Player) :-
    isEven(Player),
    findall(Player,piece(Player,_,9),[_|_]).

%/----------------------------------------/

% Predicate that checks how many pieces a Player has 
% +Player
% +Value
value(Player,Value) :-
    list_pieces(Player,L),
    length(L,Value).

%/----------------------------------------/

% Predicates that checks if the enemy doesn't have any pieces left
% +Player
check_other_player_number_pieces(Player) :-
    \+isEven(Player),
    OtherPlayer is Player + 1,
    value(OtherPlayer,Value),
    Value =:= 0. 

check_other_player_number_pieces(Player) :-
    isEven(Player),
    OtherPlayer is Player - 1,
    value(OtherPlayer,Value),
    Value =:= 0. 

%/----------------------------------------/

% Predicate that determines the number of pieces the player can move,
% according to the number of pieces they have left
% +Player
% -N -> Number of iterations
get_number_plays(Player,N) :-
    findall(_,piece(Player,_,_),List),
    length(List,N1),
    N1 >= 3,
    N is 3.

get_number_plays(Player,N) :-
    findall(_,piece(Player,_,_),List),
    length(List,N1),
    N1 = 2,
    N is 2.

get_number_plays(Player,N) :-
    findall(_,piece(Player,_,_),List),
    length(List,N1),
    N1 = 1,
    N is 1.

%/----------------------------------------/

% Recursive predicate where the user will chose which pieces
% they want to move
% +Gamestate -> Current board
% +Player
% +Pieces -> Accumulator of the chosen pieces
% -ChosenPieces -> List with the coordinates of the chosen pieces
% ?N -> Number of iterations
choose_pieces_rec(Gamestate,Player,Pieces,ChosenPieces,N) :-
    N > 0,
    format('\n#~d Which piece do you want to move forward? ("[A-Q][1-9])".\n',[4-N]),read(Cords),
    check_cords(Cords,Player,X,Y),
    N1 is N - 1,

    choose_pieces_rec(Gamestate,Player,[[X,Y]|Pieces],ChosenPieces,N1).

choose_pieces_rec(_,_,Pieces,Pieces,0).

%/----------------------------------------/

% Predicates that, according to the direction chosen in the AI's turn (hard difficulty)
% will store the new coordinates of the spaces in a list.
choose_left_pieces([],NewPieces,NewPieces).

choose_left_pieces([[[X1,Y1]]|Tail],Acc,NewPieces) :-
    choose_left_pieces(Tail,[[X1,Y1]|Acc],NewPieces).

choose_left_pieces([[[X1,Y1],_]|Tail],Acc,NewPieces) :-
    choose_left_pieces(Tail,[[X1,Y1]|Acc],NewPieces).

choose_right_pieces([],NewPieces,NewPieces).

choose_right_pieces([[[X2,Y2]]|Tail],Acc,NewPieces) :-
    choose_right_pieces(Tail,[[X2,Y2]|Acc],NewPieces).

choose_right_pieces([[_,[X2,Y2]]|Tail],Acc,NewPieces) :-
    choose_right_pieces(Tail,[[X2,Y2]|Acc],NewPieces).

%/----------------------------------------/

% Predicate that checks if the game is over due to the player reaching the enemy's side.
% +Player
game_over(Player) :-
    check_reached_other_side(Player).

% Predicate that checks if the game is over due to the enemy not having anymore pieces.
% +Player
game_over(Player) :-
    check_other_player_number_pieces(Player).

%/----------------------------------------/