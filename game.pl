:- consult('game_logic.pl').

%play game
%play

first_player_move(Board) :-
    display_board(Board),
    write('---------------------------------------------------------'),
    write('\nPLAYER 1 -\n'),
    write('\nIn which direction do you want to move?\n'),
    write('l - Left // r - Right\n'),
    get_char(Dir), %lidar com erros!
    choose_pieces(Board,1,Dir,3),
    write('\n---------------------------------------------------------\n').

second_player_move(Board) :-
    display_board(Board),
    write('---------------------------------------------------------'),
    write('\nPLAYER 2 -\n'),
    choose_pieces(Board,2,Dir,3),
    write('\n---------------------------------------------------------\n').

choose_pieces(_,_,_,_,0).
choose_pieces(Board,Player,Dir,N) :-
    repeat,
    format('\n#~d Which piece do you want to move forward? ([A,Q][1,9])\n',[4-N]),read(Cords),
    check_cords(Cords,Player,X,Y),
    move_piece(Board,Player,X,Y),
    N1 is N - 1,
    choose_pieces(Board,Player,N1).

move_piece(Board,Player,X,Y,Dir) :-
    repeat,
    check_valid_space(Board,X,Y,Dir),

game(Board) :-
    first_player_move(Board),
    second_player_move(Board).


%start game
start_game :-
    empty_board(Initial_board),
    game(Initial_board).
