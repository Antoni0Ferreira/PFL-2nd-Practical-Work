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
piece(1,8,8).
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
    get_char(Dir),skip_line,
    valid_direction(Dir).

check_existence([XInput,YInput],Player,X,Y) :-

    char_code(XLetter,XInput),
    translate_letter(XLetter,X),
    Y is YInput - 48,

    write(X),
    write('//'),
    write(Y),nl,
    findall(Player,piece(Player,X,Y),[_|_]).

check_cords([XInput,YInput],Player,X,Y) :-
    XInput >= 65,
    XInput =< 81,
    YInput >= 49,
    YInput =< 57,
    write('\ndentro do check_cords!\n'),
    check_existence([XInput,YInput],Player,X,Y).

check_valid_space(Board,1,[X,Y]) :-
    write('\ndentro do check_valid_space\n'),
    write([X,Y]),nl,
    get_board_value(Board,Y,X,Value),
    valid_spaces('+',Value).

check_valid_space(Board,2,[X,Y]) :-
    write('\ndentro do check_valid_space\n'),
    write([X,Y]),nl,
    get_board_value(Board,Y,X,Value),
    valid_spaces('o',Value).

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

get_new_space(1,[X,Y],'l',[X1,Y1]) :-
    write('\ndentro do get_new_space\n'),
    X1 is X - 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y - 1,
    Y1 >= 1,
    Y1 =< 9.

get_new_space(1,[X,Y],'r',[X1,Y1]) :-
    write('\ndentro do get_new_space\n'),
    X1 is X + 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y - 1,
    Y1 >= 1,
    Y1 =< 9.

get_new_space(2,[X,Y],'l',[X1,Y1]) :-
    write('\ndentro do get_new_space\n'),
    X1 is X - 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y + 1,
    Y1 >= 1,
    Y1 =< 9.

get_new_space(2,[X,Y],'r',[X1,Y1]) :-
    write('\ndentro do get_new_space\n'),
    X1 is X + 1,
    X1 >= 0,
    X1 =< 18,
    Y1 is Y + 1,
    Y1 >= 1,
    Y1 =< 9.

check_reached_other_side(Player) :-
    \+isEven(Player),
    findall(Player,piece(Player,_,1),[_|_]).

check_reached_other_side(Player) :-
    isEven(Player),
    findall(Player,piece(Player,_,9),[_|_]).

check_other_player_number_pieces(Player) :-
    \+isEven(Player),
    OtherPlayer is Player + 1,
    \+findall(OtherPlayer,piece(OtherPlayer,_,_),[_|_]).

check_other_player_number_pieces(Player) :-
    isEven(Player),
    OtherPlayer is Player - 1,
    \+findall(OtherPlayer,piece(OtherPlayer,_,_),[_|_]).

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

check_victory(Player) :-
    check_reached_other_side(Player),
    write('ueee').

check_victory(Player) :-
    check_other_player_number_pieces(Player),
    write('ueee!!!').