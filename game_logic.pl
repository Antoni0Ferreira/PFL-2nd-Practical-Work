:- consult('utilities.pl').
:- consult('board.pl').

%piece(Player,PosX,PosY)
:- dynamic piece/3.

piece(1,5,9).
piece(1,7,9).
piece(1,9,9).
piece(1,11,9).
piece(1,13,9).
piece(1,6,8).
piece(1,8,9).
piece(1,10,8).
piece(1,12,8).
piece(1,7,7).
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

valid_spaces('+','_').
valid_spaces('o','_').
valid_spaces('o','+').
valid_spaces('+','o').

valid_direction('r').
valid_direction('l').

get_direction(Dir) :-
    repeat,
    write('\nIn which direction do you want to move?\n'),
    write('l - Left // r - Right\n'),
    get_char(Dir),
    valid_direction(Dir).

check_existence([Head|Tail],Player,X,Y) :-

    char_code(X_letter,Head),
    translate_letter(X_letter,X),

    last_element(Tail,Y_ascii),
    Y is Y_ascii - 48,

    write(X),
    write('//'),
    write(Y),nl,
    findall(Player,piece(Player,X,Y),[_|_]).

check_Y([Head|Tail]) :-
    Head >= 49,
    Head =< 57,
    length(Tail,N),
    N == 0.

check_cords([Head|Tail],Player,X,Y) :-
    Head >= 65,
    Head =< 81,
    check_Y(Tail),
    check_existence([Head|Tail],Player,X,Y).

check_valid_space(Board,1,[X,Y],'l',[X1,Y1]) :-
    write('\ndentro do check_valid_space\n'),
    X1 is X - 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y - 1,
    Y1 >= 1,
    Y1 =< 9,
    get_board_value(Board,Y1,X1,Value),
    valid_spaces('+',Value).

check_valid_space(Board,1,[X,Y],'r',[X1,Y1]) :-
    write('\ndentro do check_valid_space\n'),
    X1 is X + 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y - 1,
    Y1 >= 1,
    Y1 =< 9,
    get_board_value(Board,Y1,X1,Value),
    valid_spaces('+',Value).

check_valid_space(Board,2,[X,Y],'l',[X1,Y1]) :-
    write('\ndentro do check_valid_space\n'),
    X1 is X - 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y + 1,
    Y1 >= 1,
    Y1 =< 9,
    get_board_value(Board,Y1,X1,Value),
    valid_spaces('o',Value).

check_valid_space(Board,2,[X,Y],'r',[X1,Y1]) :-
    write('\ndentro do check_valid_space\n'),
    X1 is X + 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y + 1,
    Y1 >= 1,
    Y1 =< 9,
    get_board_value(Board,Y1,X1,Value),
    valid_spaces('o',Value).

move_piece(Board,1,[X,Y],NewBoard) :-
    write('\ndentro do move_piece\n'),
    format('X- ~d // Y- ~d\n',[X,Y]),
    replace_board_value(Board,Y,X,'+',NewBoard),
    assert(piece(1,X,Y)).

move_piece(Board,2,[X,Y],NewBoard) :-
    write('\ndentro do move_piece\n'),
    format('X- ~d // Y- ~d\n',[X,Y]),
    replace_board_value(Board,Y,X,'o',NewBoard),
    assert(piece(2,X,Y)).

clean_space(Board,Player,[X,Y],NewBoard) :-
    write('\ndentro do clean_space\n'),
    format('X- ~d // Y- ~d\n',[X,Y]),
    replace_board_value(Board,Y,X,'_',NewBoard),
    retract(piece(Player,X,Y)).