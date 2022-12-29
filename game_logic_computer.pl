:- consult('game_logic.pl').

%/----------------------------------------/

% Predicates that, given a set of coordinates, determine
% the possible set of moves and update the database with the predicates next_piece(X,Y,Weight). It takes in
% consideration the player and the restrictions imposed.
% If the next move is an empty space, then the weight given to it will be 1.
% If the the next move is the piece of the enemy player, then the weight will be 2.
% If there's only one possible move, that move will receive the weight of 3 and the other move,
% which is invalid, will receive the weight of 0.
% +Board -> Current Board
% +Player
% +[X,Y] -> Coordinates
% -PossiblePieces -> List of the coordinates of possible moves/next pieces
valid_moves(Board,1,[X,Y],[[X11,Y1],[X12,Y1]]) :-
    X11 is (X - 1),
    X12 is (X + 1),
    Y1 is (Y - 1),
    check_valid_space(Board,1,[X11,Y1]),
    check_valid_space(Board,1,[X12,Y1]),
    piece(2,X11,Y1),
    piece(2,X12,Y1),
    assert(next_piece(X11,Y1,2)),
    assert(next_piece(X12,Y1,2)).

valid_moves(Board,1,[X,Y],[[X11,Y1],[X12,Y1]]) :-
    X11 is (X - 1),
    X12 is (X + 1),
    Y1 is (Y - 1),
    check_valid_space(Board,1,[X11,Y1]),
    check_valid_space(Board,1,[X12,Y1]),
    piece(2,X11,Y1),
    \+piece(2,X12,Y1),
    assert(next_piece(X11,Y1,2)),
    assert(next_piece(X12,Y1,1)).

valid_moves(Board,1,[X,Y],[[X11,Y1],[X12,Y1]]) :-
    X11 is (X - 1),
    X12 is (X + 1),
    Y1 is (Y - 1),
    check_valid_space(Board,1,[X11,Y1]),
    check_valid_space(Board,1,[X12,Y1]),
    \+piece(2,X11,Y1),
    piece(2,X12,Y1),
    assert(next_piece(X11,Y1,1)),
    assert(next_piece(X12,Y1,2)).

valid_moves(Board,1,[X,Y],[[X11,Y1],[X12,Y1]]) :-
    X11 is (X - 1),
    X12 is (X + 1),
    Y1 is (Y - 1),
    check_valid_space(Board,1,[X11,Y1]),
    check_valid_space(Board,1,[X12,Y1]),
    \+piece(2,X11,Y1),
    \+piece(2,X12,Y1),
    assert(next_piece(X11,Y1,1)),
    assert(next_piece(X12,Y1,1)).


% ----//----

valid_moves(Board,1,[X,Y],[[X11,Y1],[X12,Y1]]) :-
    X11 is (X - 1),
    X12 is (X + 1),
    Y1 is (Y - 1),
    check_valid_space(Board,1,[X11,Y1]),
    \+check_valid_space(Board,1,[X12,Y1]),
    assert(next_piece(X11,Y1,3)),
    assert(next_piece(X12,Y1,0)).

% ----//----

valid_moves(Board,1,[X,Y],[[X11,Y1],[X12,Y1]]) :-
    X11 is (X - 1),
    X12 is (X + 1),
    Y1 is (Y - 1),
    \+check_valid_space(Board,1,[X11,Y1]),
    check_valid_space(Board,1,[X12,Y1]),
    assert(next_piece(X11,Y1,0)),
    assert(next_piece(X12,Y1,3)).

% ----//----

valid_moves(Board,2,[X,Y],[[X21,Y2],[X22,Y2]]) :-
    X21 is (X - 1),
    X22 is (X + 1),
    Y2 is (Y + 1),
    check_valid_space(Board,2,[X21,Y2]),
    check_valid_space(Board,2,[X22,Y2]),
    piece(1,X21,Y2),
    piece(1,X22,Y2),
    assert(next_piece(X21,Y2,2)),
    assert(next_piece(X22,Y2,2)).

valid_moves(Board,2,[X,Y],[[X21,Y2],[X22,Y2]]) :-
    X21 is (X - 1),
    X22 is (X + 1),
    Y2 is (Y + 1),
    check_valid_space(Board,2,[X21,Y2]),
    check_valid_space(Board,2,[X22,Y2]),
    piece(1,X21,Y2),
    \+piece(1,X22,Y2),
    assert(next_piece(X21,Y2,2)),
    assert(next_piece(X22,Y2,1)).

valid_moves(Board,2,[X,Y],[[X21,Y2],[X22,Y2]]) :-
    X21 is (X - 1),
    X22 is (X + 1),
    Y2 is (Y + 1),
    check_valid_space(Board,2,[X21,Y2]),
    check_valid_space(Board,2,[X22,Y2]),
    \+piece(1,X21,Y2),
    piece(1,X22,Y2),
    assert(next_piece(X21,Y2,1)),
    assert(next_piece(X22,Y2,2)).

valid_moves(Board,2,[X,Y],[[X21,Y2],[X22,Y2]]) :-
    X21 is (X - 1),
    X22 is (X + 1),
    Y2 is (Y + 1),
    check_valid_space(Board,2,[X21,Y2]),
    check_valid_space(Board,2,[X22,Y2]),
    \+piece(1,X21,Y2),
    \+piece(1,X22,Y2),
    assert(next_piece(X21,Y2,1)),
    assert(next_piece(X22,Y2,1)).

% ----//----

valid_moves(Board,2,[X,Y],[[X21,Y2],[X22,Y2]]) :-
    X21 is (X - 1),
    X22 is (X + 1),
    Y2 is (Y + 1),
    check_valid_space(Board,2,[X21,Y2]),
    \+check_valid_space(Board,2,[X22,Y2]),
    assert(next_piece(X21,Y2,3)),
    assert(next_piece(X22,Y2,0)).

% ----//----

valid_moves(Board,2,[X,Y],[[X21,Y2],[X22,Y2]]) :-
    X21 is (X - 1),
    X22 is (X + 1),
    Y2 is (Y + 1),
    \+check_valid_space(Board,2,[X21,Y2]),
    check_valid_space(Board,2,[X22,Y2]),
    assert(next_piece(X21,Y2,0)),
    assert(next_piece(X22,Y2,3)).

valid_moves(_,_,_,[]).

%/----------------------------------------/

% Predicates where the AI will select the best direction in which
% the pieces should move to
% +PossiblePieces
% +Left -> Accumulator of the weight of the left possible pieces
% +Right -> Accumulator of the weight of the right possible pieces
% LeftTotal -> Total Left weight
% RightTotal -> Total Right weight
find_direction([],Left,Right,Left,Right).

find_direction([[[X1,Y1]]|Tail],Left,Right,LeftTotal,RightTotal) :-
    next_piece(X1,Y1,Weight1),
    LeftTotal1 is Left + Weight1,
    RightTotal1 is Right + Weight1,
    find_direction(Tail,LeftTotal1,RightTotal1,LeftTotal,RightTotal).

find_direction([[[X1,Y1],[X2,Y2]]|Tail],Left,Right,LeftTotal,RightTotal) :-
    next_piece(X1,Y1,Weight1),
    next_piece(X2,Y2,Weight2),
    LeftTotal1 is Left + Weight1,
    RightTotal1 is Right + Weight2,
    find_direction(Tail,LeftTotal1,RightTotal1,LeftTotal,RightTotal).

%/----------------------------------------/

% Predicate where the AI will randomly get the direction in which
% it wants to move the pieces
% -Dir -> Direction
get_direction_computer(Dir) :-
    DirList = ['l','r'],
    random_member(Dir,DirList).

%/----------------------------------------/

% Recursive predicate where the AI (easy difficulty) will randomly chose which pieces
% they want to move
% +Gamestate -> Current board
% +Player
% +Pieces -> Accumulator of the chosen pieces
% -ChosenPieces -> List with the coordinates of the chosen pieces
% ?N -> Number of iterations
choose_pieces_computer_rec(Gamestate,Player,Pieces,ChosenPieces,N) :-
    N > 0,
    list_pieces(Player,PossiblePieces),
    length(PossiblePieces,Length),
    random(0,Length,Index),
    nth0(Index,PossiblePieces,piece(Player,X,Y)),
    N1 is N - 1,

    choose_pieces_computer_rec(Gamestate,Player,[[X,Y]|Pieces],ChosenPieces,N1).

choose_pieces_computer_rec(_,_,ChosenPieces,ChosenPieces,0).

%/----------------------------------------/

% Predicate to obtain the coordinates X and Y of a piece 
get_piece_values(piece(_, X, Y), [X, Y]).

get_piece_values(Pieces, Pairs) :-
    maplist(get_piece_values, Pieces, Pairs).

%/----------------------------------------/

% Predicate used to get the first N pieces of a list
% +[Head|Tail] -> List of pieces
% ?Acc -> Accumulator
% -Pieces -> List of the N first pieces
% ?N -> Number of iterations
get_pieces([Head|Tail],Acc,Pieces, N) :-
    N > 0,
    N1 is N - 1,
    get_pieces(Tail,[Head|Acc],Pieces,N1).

get_pieces(_,Pieces,Pieces,0).

%/----------------------------------------/

% Predicate used in the hard difficulty levels by the AI to choose the
% best possible pieces at the moment. After obtaining all the possible pieces,
% the list will be sorted using quicksort and the first N pieces will be the chosen ones
% +Player
% -ChosenPieces
% ?N -> Number of iterations
choose_best_pieces(Player,ChosenPieces,N) :-

    list_pieces(Player,PossiblePieces),
    quicksort_pieces(Player,PossiblePieces,SortedPieces),

    get_pieces(SortedPieces,[],Pieces,N),
    get_piece_values(Pieces,ChosenPieces).
