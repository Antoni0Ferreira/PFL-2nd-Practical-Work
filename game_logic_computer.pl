:- consult('game_logic.pl').

%/----------------------------------------/

valid_moves(Board,1,[X,Y],[[X11,Y1],[X12,Y1]]) :-
    % format('\n[X,Y] - ~w',[[X,Y]]),
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
    % format('\n[X,Y] - ~w',[[X,Y]]),
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
    % format('\n[X,Y] - ~w',[[X,Y]]),
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
    % format('\n[X,Y] - ~w',[[X,Y]]),
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

valid_moves(Board,1,[X,Y],[[X11,Y1]]) :-
    % format('\n[X,Y] - ~w',[[X,Y]]),
    X11 is (X - 1),
    X12 is (X + 1),
    Y1 is (Y - 1),
    check_valid_space(Board,1,[X11,Y1]),
    \+check_valid_space(Board,1,[X12,Y1]),
    assert(next_piece(X11,Y1,3)).

% ----//----

valid_moves(Board,1,[X,Y],[[X12,Y1]]) :-
    % format('\n[X,Y] - ~w',[[X,Y]]),
    X11 is (X - 1),
    X12 is (X + 1),
    Y1 is (Y - 1),
    \+check_valid_space(Board,1,[X11,Y1]),
    check_valid_space(Board,1,[X12,Y1]),
    assert(next_piece(X12,Y1,3)).

% ----//----

valid_moves(Board,2,[X,Y],[[X21,Y2],[X22,Y2]]) :-
    % format('\n[X,Y] - ~w',[[X,Y]]),
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
    % format('\n[X,Y] - ~w',[[X,Y]]),
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
    % format('\n[X,Y] - ~w',[[X,Y]]),
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
    % format('\n[X,Y] - ~w',[[X,Y]]),
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

valid_moves(Board,2,[X,Y],[[X21,Y2]]) :-
    % format('\n[X,Y] - ~w',[[X,Y]]),
    X21 is (X - 1),
    X22 is (X + 1),
    Y2 is (Y + 1),
    check_valid_space(Board,2,[X21,Y2]),
    \+check_valid_space(Board,2,[X22,Y2]),
    assert(next_piece(X21,Y2,3)).

% ----//----

valid_moves(Board,2,[X,Y],[[X22,Y2]]) :-
    % format('\n[X,Y] - ~w',[[X,Y]]),
    X21 is (X - 1),
    X22 is (X + 1),
    Y2 is (Y + 1),
    \+check_valid_space(Board,2,[X21,Y2]),
    check_valid_space(Board,2,[X22,Y2]),
    assert(next_piece(X22,Y2,3)).

valid_moves(_,_,_,[]).

%/----------------------------------------/

find_direction([],Left,Right,Left,Right).

find_direction([[[X1,Y1]]|Tail],Left,Right,LeftTotal,RightTotal) :-
    % write('\nEstou dentro do find_next_piece\n'),
    next_piece(X1,Y1,Weight1),
    % list_next_pieces([X1,Y1],[X2,Y2],_,[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]),
    % list_next([X1,Y1],[X2,Y2],[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]),
    LeftTotal1 is Left + Weight1,
    RightTotal1 is Right + Weight1,
    find_direction(Tail,LeftTotal1,RightTotal1,LeftTotal,RightTotal).
    % format('\nLIST NEXT PIECES 1 - ~w\n',[[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]]).

find_direction([[[X1,Y1],[X2,Y2]]|Tail],Left,Right,LeftTotal,RightTotal) :-
    % write('\nEstou dentro do find_next_piece\n'),
    next_piece(X1,Y1,Weight1),
    next_piece(X2,Y2,Weight2),
    % list_next_pieces([X1,Y1],[X2,Y2],_,[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]),
    % list_next([X1,Y1],[X2,Y2],[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]),
    LeftTotal1 is Left + Weight1,
    RightTotal1 is Right + Weight2,
    find_direction(Tail,LeftTotal1,RightTotal1,LeftTotal,RightTotal).
    % format('\nLIST NEXT PIECES 1 - ~w\n',[[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]]).

find_next_piece([],_).
find_next_piece([[X1,Y1]],[X1,Y1]).
find_next_piece([[X1,Y1],[X2,Y2]],[X1,Y1]) :-
    % write('\nEstou dentro do find_next_piece\n'),
    next_piece(X1,Y1,Weight1),
    next_piece(X2,Y2,Weight2),
    % list_next_pieces([X1,Y1],[X2,Y2],_,[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]),
    % list_next([X1,Y1],[X2,Y2],[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]),
    Weight1 >= Weight2.
    % format('\nLIST NEXT PIECES 1 - ~w\n',[[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]]).

find_next_piece([[X1,Y1],[X2,Y2]],[X2,Y2]) :-
    % write('\nEstou dentro do find_next_piece\n'),
    % list_next_pieces([X1,Y1],[X2,Y2],_,[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]),
    next_piece(X1,Y1,Weight1),
    next_piece(X2,Y2,Weight2),
    % list_next([X1,Y1],[X2,Y2],[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]),
    Weight2 > Weight1.
    % format('\nLIST NEXT PIECES 2 - ~w\n',[[next_piece(X1,Y1,Weight1),next_piece(X2,Y2,Weight2)]]).

%/----------------------------------------/

get_direction_computer(Dir) :-
    DirList = ['l','r'],
    random_member(Dir,DirList).

%/----------------------------------------/

choose_pieces_computer_rec(Gamestate,Player,Pieces,ChosenPieces,N) :-
    N > 0,
    % write('\ndentro do choose_pieces_computer_rec\n'),
    list_pieces(Player,PossiblePieces),
    % format('\nPossiblePieces - ~w\n',[PossiblePieces]),
    length(PossiblePieces,Length),
    % format('\nlength - ~d',Length),
    random(0,Length,Index),
    % format('\nindex - ~d\n',[Index]),
    nth0(Index,PossiblePieces,piece(Player,X,Y)),
    % format('\n[X,Y] - ~w\n',piece(Player,X,Y)),
    N1 is N - 1,

    choose_pieces_computer_rec(Gamestate,Player,[[X,Y]|Pieces],ChosenPieces,N1).

choose_pieces_computer_rec(_,_,ChosenPieces,ChosenPieces,0).

%/----------------------------------------/

get_piece_values(piece(_, X, Y), [X, Y]).

get_piece_values(Pieces, Pairs) :-
    maplist(get_piece_values, Pieces, Pairs).

%/----------------------------------------/

get_pieces([Head|Tail],Acc,Pieces, N) :-
    N > 0,
    % write('\nDentro do get_pieces!\n'),
    N1 is N - 1,
    get_pieces(Tail,[Head|Acc],Pieces,N1).

get_pieces(_,Pieces,Pieces,0).

%/----------------------------------------/

sort_pieces(1,PossiblePieces,SortedPieces) :-
    sort(PossiblePieces,SortedPieces).

sort_pieces(2,PossiblePieces,SortedPieces) :-
    sort(PossiblePieces,SortedPieces1),
    reverse(SortedPieces1,SortedPieces).

%/----------------------------------------/

choose_best_pieces(Player,ChosenPieces,N) :-
    % write('\nDentro do choose_best_pieces!\n'),
    list_pieces(Player,PossiblePieces),

    % format('\nPOSSIBLE PIECES - ~w\n',[PossiblePieces]),

    quicksort_pieces(Player,PossiblePieces,SortedPieces),
    % format('\nSORTED PIECES - ~w\n',[SortedPieces]),

    get_pieces(SortedPieces,[],Pieces,N),
    % format('\nPIECES - ~w\n',[Pieces]),

    get_piece_values(Pieces,ChosenPieces).

    % format('\nCHOSEN PIECES - ~w\n',[ChosenPieces]).
