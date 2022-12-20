:- consult('game_logic.pl').

%play game
%play


move(Gamestate,Player,NewGamestate) :-
    display_game(Gamestate),
    write('---------------------------------------------------------'),
    format('\nPLAYER ~d -\n',[Player]),

    get_direction(Dir),

    get_number_plays(Player,N),

    format('\nN- ~d\n',[N]),

    choose_pieces(Gamestate,Player,[],Pieces,N),
    get_new_spaces(Player,Pieces,Dir,[],NewPieces,N),

    write('vou para o move_pieces!\n'),
    write(Pieces),nl,

    clean_game(Gamestate,Player,Pieces,NewGamestate1,N),
    write('\ndepois do clean_game\n'),
    check_valid_spaces(NewGamestate1,Player,NewPieces,N),

    move_pieces(NewGamestate1,Player,Dir,NewPieces,NewGamestate,N),
    check_attacks(Player,NewPieces,N),

    write('---------------------------------------------------------'),skip_line,!.

check_valid_spaces(Gamestate,Player,[Head|Tail],N) :-

    N > 0,
    write([Head|Tail]),nl,
    write(Head),
    check_valid_space(Gamestate,Player,Head),
    N1 is N - 1,
    write('\nue\n'),
    check_valid_spaces(Gamestate,Player,Tail,N1).

check_valid_spaces(_,_,[],0).

check_attacks(Player,[[X,Y]|Tail],N) :-

    N > 0,
    check_attack(Player,[X,Y]),
    N1 is N - 1,
    check_attacks(Player,Tail,N1).

check_attacks(_,_,0).


clean_game(Gamestate,Player,[[X,Y]|Tail],NewGamestate,N) :-

    N > 0,
    write([[X,Y]|Tail]),
    clean_space(Gamestate,Player,[X,Y],NewGamestate1),
    N1 is N - 1,
    clean_game(NewGamestate1,Player,Tail,NewGamestate,N1).

clean_game(Gamestate,_,[],Gamestate,0).

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

choose_pieces(Gamestate,Player,Pieces,ChosenPieces,N) :-
    repeat,
    choose_pieces_rec(Gamestate,Player,Pieces,ChosenPieces,N).

choose_pieces_rec(Gamestate,Player,Pieces,ChosenPieces,N) :-
    N > 0,
    format('\n#~d Which piece do you want to move forward? ([A,Q][1,9])\n',[4-N]),read(Cords),
    check_cords(Cords,Player,X,Y),
    write('\ndepois do check_cords\n'),
    N1 is N - 1,

    choose_pieces_rec(Gamestate,Player,[[X,Y]|Pieces],ChosenPieces,N1).

choose_pieces_rec(_,_,Pieces,Pieces,0).


get_new_spaces(Player,[[X,Y]|Tail],Dir,Acc,NewPieces,N) :-
    N > 0,
    get_new_space(Player,[X,Y],Dir,[X1,Y1]),
    N1 is N - 1,

    get_new_spaces(Player,Tail,Dir,[[X1,Y1]|Acc],NewPieces,N1).

get_new_spaces(_,[],_,Pieces,Pieces,0).



game(FinalGamestate,N,FinalGamestate) :-
    isEven(N),
    check_victory(1),
    write('\nCongrats, player 1 won!\n').

game(Gamestate,N,FinalGamestate) :-
    isEven(N),
    \+check_victory(2),
    move(Gamestate,2,NewGamestate),
    N1 is N + 1,nl,listing(piece),nl,
    game(NewGamestate,N1,FinalGamestate).    

game(FinalGamestate,N,FinalGamestate) :-
    \+isEven(N),
    check_victory(2),
    write('\nCongrats, player 2 won!\n').

game(Gamestate,N,FinalGamestate) :-
    \+isEven(N),
    \+check_victory(1),
    move(Gamestate,1,NewGamestate),
    N1 is N + 1,nl,listing(piece),nl,
    game(NewGamestate,N1,FinalGamestate).



%start game
start_game :-
    empty_board(InitialGamestate),
    game(InitialGamestate,1,FinalGamestate),
    display_game(FinalGamestate).
