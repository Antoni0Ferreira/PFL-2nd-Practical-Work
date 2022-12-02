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

check_existence([Head|Tail],Player) :-

    char_code(X_letter,Head),
    translate_letter(X_letter,X),

    lastElement(Tail,Y_ascii),
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

check_cords([Head|Tail],Player) :-
    Head >= 65,
    Head =< 81,
    check_Y(Tail),
    check_existence([Head|Tail],Player).