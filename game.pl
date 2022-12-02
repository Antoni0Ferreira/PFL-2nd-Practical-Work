:- consult('game_logic.pl').

%play game
%play

first_player_move(Board) :-
    display_board(Board),
    write('---------------------------------------------------------'),
    write('\nPLAYER 1 -\n'),
    choose_piece(Board,1,3),
    write('\n---------------------------------------------------------\n').

second_player_move(Board) :-
    display_board(Board),
    write('---------------------------------------------------------'),
    write('\nPLAYER 2 -\n'),
    choose_piece(Board,2,3),
    write('\n---------------------------------------------------------\n').

choose_piece(_,_,0).
choose_piece(Board,Player,N) :-
    repeat,
    format('\n#~d Which piece do you want to move forward? ([A,Q][1,9])\n',[4-N]),read(Cords),
    check_cords(Cords,Player),
    N1 is N - 1,
    choose_piece(Board,Player,N1).

game(Board) :-
    first_player_move(Board),
    second_player_move(Board).


%start game
start_game :-
    empty_board(Initial_board),
    game(Initial_board).
