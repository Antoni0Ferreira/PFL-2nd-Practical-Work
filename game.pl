:- consult('game_logic.pl').

%play game
%play

move(Gamestate,Player,NewGamestate) :-
    display_game(Gamestate),
    write('---------------------------------------------------------'),
    format('\nPLAYER ~d -\n',[Player]),

    get_direction(Dir),
    format('\nDirection - ~w',[Dir]),

    choose_pieces(Gamestate,Player,Dir,[],Pieces,3),
    check_valid_spaces(Gamestate,Player,Pieces,Dir,[],NewPieces,3),

    write('vou para o move_pieces!\n'),
    write(Pieces),nl,

    clean_game(Gamestate,Player,Pieces,NewGamestate1,3),
    move_pieces(NewGamestate1,Player,Dir,NewPieces,NewGamestate,3),

    write('---------------------------------------------------------'),!.

check_valid_spaces(Gamestate,Player,[[X,Y]|Tail],Dir,Acc,NewPieces,N) :-

    N > 0,
    check_valid_space(Gamestate,Player,[X,Y],Dir,[X1,Y1]),
    N1 is N - 1,
    check_valid_spaces(Gamestate,Player,Tail,Dir,[[X1,Y1]|Acc],NewPieces,N1).

check_valid_spaces(_,_,_,_,Pieces,Pieces,0).

clean_game(Gamestate,Player,[[X,Y]|Tail],NewGamestate,N) :-

    N > 0,
    clean_space(Gamestate,Player,[X,Y],NewGamestate1),
    N1 is N - 1,
    clean_game(NewGamestate1,Player,Tail,NewGamestate,N1).

clean_game(Gamestate,_,Gamestate,0).

move_pieces(Gamestate,Player,Dir,[[X,Y]|Tail],NewGamestate,N) :-

    N > 0,
    write('\ndentro do move_pieces\n'),
    write([[X,Y]|Tail]),


    move_piece(Gamestate,Player,[X,Y],NewGamestate1),
    display_game(NewGamestate1),
    write('\ndepois do move_piece\n'),
    N1 is N - 1,
    format('N1 is ~d\n',[N1]),
    move_pieces(NewGamestate1,Player,Dir,Tail,NewGamestate,N1).

move_pieces(Gamestate,_,_,_,Gamestate,0).


choose_pieces(Gamestate,Player,Dir,Pieces,ChosenPieces,N) :-
    N > 0,
    format('\n#~d Which piece do you want to move forward? ([A,Q][1,9])\n',[4-N]),read(Cords),
    check_cords(Cords,Player,X,Y),
    N1 is N - 1,

    choose_pieces(Gamestate,Player,Dir,[[X,Y]|Pieces],ChosenPieces,N1).

choose_pieces(_,_,_,Pieces,Pieces,0).

game(Gamestate,N,NewGamestate) :-
    isEven(N),
    move(Gamestate,2,NewGamestate),
    N1 is N + 1,
    game(NewGamestate,N1,NewGamestate).

game(Gamestate,N,NewGamestate) :-
    \+isEven(N),
    move(Gamestate,1,NewGamestate),
    N1 is N + 1,
    game(NewGamestate,N1,NewGamestate).


%start game
start_game :-
    empty_board(InitialGamestate),
    game(InitialGamestate,1,FinalGamestate).
