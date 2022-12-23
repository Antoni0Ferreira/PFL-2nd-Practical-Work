:- consult('game_logic.pl').
:- consult('game_logic_computer.pl').

%play game
%play


move(Gamestate,Player,NewGamestate) :-
    display_game(Gamestate),
    write('---------------------------------------------------------'),
    format('\nPLAYER ~d -\n',[Player]),

    get_direction(Dir),

    get_number_plays(Player,N),

    format('\nN- ~d\n',[N]),

    choose_pieces(Gamestate,Player,Dir,[],NewPieces,NewGamestate1,N),

    move_pieces(NewGamestate1,Player,NewPieces,NewGamestate,N),
    check_attacks(Player,NewPieces,N),

    write('---------------------------------------------------------'),skip_line,!.

%/----------------------------------------/

choose_move(Gamestate,Player,1,NewGamestate) :-
    display_game(Gamestate),
    write('---------------------------------------------------------'),
    format('\nCOMPUTER - PLAYER ~d -\n',[Player]),

    get_direction_computer(Dir),
    format('\nDir - ~w\n',[Dir]),

    get_number_plays(Player,N),
    format('\nN - ~d\n',[N]),

    choose_pieces_computer(Gamestate,Player,Dir,[],NewPieces,NewGamestate1,N),

    move_pieces(NewGamestate1,Player,NewPieces,NewGamestate,N),
    check_attacks(Player,NewPieces,N),

    write('---------------------------------------------------------'),!.


choose_move(Gamestate,Player,2,NewGamestate) :-
    display_game(Gamestate),
    write('---------------------------------------------------------'),
    format('\nCOMPUTER - PLAYER ~d -\n',[Player]),

    get_number_plays(Player,N),
    format('\nN - ~d\n',[N]),

    choose_best_pieces(Player,ChosenPieces,N),
    write('\nAfter choose_best_pieces\n'),

    get_valid_moves(Gamestate,Player,ChosenPieces,[],NewPieces,N),
    write('\nAfter get_valid_moves\n'),
    format('\nNEW PIECES - ~w\n',[NewPieces]),
    
    find_next_pieces(NewPieces,[],BestPieces,N),
    write('\nAfter find_next_pieces\n'),
    format('\nBEST PIECES - ~w\n',[BestPieces]),

    clean_game(Gamestate,Player,ChosenPieces,NewGamestate1,N),
    move_pieces(NewGamestate1,Player,BestPieces,NewGamestate,N),

    write('---------------------------------------------------------'),!.

%/----------------------------------------/

check_valid_spaces(Gamestate,Player,[Head|Tail],N) :-

    N > 0,
    format('\nList - ~w\n',[[Head|Tail]]),
    check_valid_space(Gamestate,Player,Head),
    N1 is N - 1,
    check_valid_spaces(Gamestate,Player,Tail,N1).

check_valid_spaces(_,_,[],0).

%/----------------------------------------/

check_attacks(Player,[[X,Y]|Tail],N) :-

    N > 0,
    check_attack(Player,[X,Y]),
    N1 is N - 1,
    check_attacks(Player,Tail,N1).

check_attacks(_,_,0).

%/----------------------------------------/

clean_game(Gamestate,Player,[[X,Y]|Tail],NewGamestate,N) :-

    N > 0,
    format('\nCLEAN THESE SPACES - ~w\n',[[[X,Y]|Tail]]),
    clean_space(Gamestate,Player,[X,Y],NewGamestate1),
    N1 is N - 1,
    clean_game(NewGamestate1,Player,Tail,NewGamestate,N1).

clean_game(Gamestate,_,[],Gamestate,0).

%/----------------------------------------/

prepare_move(Gamestate,[[X,Y]|Tail],NewGamestate,N) :-

    N > 0,
    prepare_space(Gamestate,[X,Y],NewGamestate1),
    N1 is N - 1,
    prepare_move(NewGamestate1,Tail,NewGamestate,N1).

prepare_move(Gamestate,[],Gamestate,0).

%/----------------------------------------/

move_pieces(Gamestate,Player,[[X,Y]|Tail],NewGamestate,N) :-

    N > 0,
    % write('\ndentro do move_pieces\n'),
    write([[X,Y]|Tail]),

    move_piece(Gamestate,Player,[X,Y],NewGamestate1),
    display_game(NewGamestate1),
    % write('\ndepois do move_piece\n'),
    N1 is N - 1,
    % format('N1 is ~d\n',[N1]),
    move_pieces(NewGamestate1,Player,Tail,NewGamestate,N1).

move_pieces(Gamestate,_,_,Gamestate,0).

%/----------------------------------------/

choose_pieces(Gamestate,Player,Dir,Pieces,NewPieces,NewGamestate,N) :-
    repeat,
    choose_pieces_rec(Gamestate,Player,Pieces,ChosenPieces,N),
    \+has_repeated_elements(ChosenPieces),
    format('\nPIECES INICIAL: ~w\n',[ChosenPieces]),

    get_new_spaces(Player,ChosenPieces,Dir,[],NewPieces,N),
    format('\nNEW PIECES: ~w\n',[NewPieces]),

    prepare_move(Gamestate,ChosenPieces,NewGamestate1,N),
    check_valid_spaces(NewGamestate1,Player,NewPieces,N),
    clean_game(Gamestate,Player,ChosenPieces,NewGamestate,N).

choose_pieces_computer(Gamestate,Player,Dir,Pieces,NewPieces,NewGamestate,N) :-
    repeat,
    choose_pieces_computer_rec(Gamestate,Player,Pieces,ChosenPieces,N),
    \+has_repeated_elements(ChosenPieces),
    format('\nPIECES INICIAL: ~w\n',[ChosenPieces]),

    get_new_spaces(Player,ChosenPieces,Dir,[],NewPieces,N),
    format('\nNEW PIECES: ~w\n',[NewPieces]),

    prepare_move(Gamestate,ChosenPieces,NewGamestate1,N),
    check_valid_spaces(NewGamestate1,Player,NewPieces,N),
    clean_game(Gamestate,Player,ChosenPieces,NewGamestate,N).

%/----------------------------------------/

get_new_spaces(Player,[[X,Y]|Tail],Dir,Acc,NewPieces,N) :-

    N > 0,
    get_new_space(Player,[X,Y],Dir,[X1,Y1]),
    N1 is N - 1,
    get_new_spaces(Player,Tail,Dir,[[X1,Y1]|Acc],NewPieces,N1).

get_new_spaces(_,[],_,Pieces,Pieces,0).

%/----------------------------------------/

get_valid_moves(Gamestate,Player,[[X,Y]|Tail],Acc,AllValidMoves,N) :-

    N > 0,
    write('\nEstou dentro do get_valid_moves!\n'),
    format('Lista - ~w',[[[X,Y]|Tail]]),
    valid_moves(Gamestate,Player,[X,Y],ValidMoves),
    N1 is N - 1,
    get_valid_moves(Gamestate,Player,Tail,[ValidMoves|Acc],AllValidMoves,N1).

get_valid_moves(_,_,[],ValidMoves,ValidMoves,0).

%/----------------------------------------/

find_next_pieces([Head|Tail],Acc,NewPieces,N) :-

    N > 0,
    write('\nEstou dentro do find_next_pieces!\n'),
    format('\nHEAD - ~w\n',[Head]),
    find_next_piece(Head,NewPiece),
    format('\nNewPiece - ~w\n',[NewPiece]),
    N1 is N - 1,
    find_next_pieces(Tail,[NewPiece|Acc],NewPieces,N1).

find_next_pieces(_,NewPieces,NewPieces,0).

%/----------------------------------------/

game(FinalGamestate,N,FinalGamestate) :-
    isEven(N),
    game_over(1),
    write('\nCongrats, player 1 won!\n').

game(Gamestate,N,FinalGamestate) :-
    isEven(N),
    \+game_over(2),
    move(Gamestate,2,NewGamestate),
    N1 is N + 1,nl,listing(piece),nl,
    game(NewGamestate,N1,FinalGamestate).    

game(FinalGamestate,N,FinalGamestate) :-
    \+isEven(N),
    game_over(2),
    write('\nCongrats, player 2 won!\n').

game(Gamestate,N,FinalGamestate) :-
    \+isEven(N),
    \+game_over(1),
    move(Gamestate,1,NewGamestate),
    N1 is N + 1,nl,listing(piece),nl,
    game(NewGamestate,N1,FinalGamestate).

%/----------------------------------------/

game_one_computer(Gamestate,Level,FinalGamestate) :-
    \+game_over(1),
    move(Gamestate,1,NewGamestate),
    \+game_over(2),
    choose_move(NewGamestate,2,Level,NewGamestate1),nl,listing(piece),nl,
    game_one_computer(NewGamestate1,Level,FinalGamestate).

game_one_computer(FinalGamestate,_,FinalGamestate) :-
    game_over(1),
    write('\nCongrats, you won!\n').

game_one_computer(Gamestate,_,FinalGamestate) :-
    \+game_over(1),
    move(Gamestate,1,FinalGamestate),
    game_over(2),
    write('\nSorry, you lost :(\n').

%/----------------------------------------/

game_two_computers(Gamestate,Level,FinalGamestate) :-
    \+game_over(1),
    choose_move(Gamestate,1,Level,NewGamestate),
    \+game_over(2),
    choose_move(NewGamestate,2,Level,NewGamestate1),
    game_two_computers(NewGamestate1,Level,FinalGamestate).

game_two_computers(FinalGamestate,_,FinalGamestate) :-
    game_over(1),
    write('\nCongrats, Computer #1 won!\n').

game_two_computers(Gamestate,Level,FinalGamestate) :-
    \+game_over(1),
    choose_move(Gamestate,1,Level,FinalGamestate),
    game_over(2),
    write('\nCongrats, Computer #2 won!\n').

%/----------------------------------------/

%game modes - start game
game_pvp :-
    empty_board(InitialGamestate),
    game(InitialGamestate,1,FinalGamestate),
    display_game(FinalGamestate).

game_pvai(Level) :-
    empty_board(InitialGamestate),
    game_one_computer(InitialGamestate,Level,FinalGamestate),
    display_game(FinalGamestate).

game_aivai(Level) :-
    empty_board(InitialGamestate),
    game_two_computers(InitialGamestate, Level, FinalGamestate),
    display_game(FinalGamestate).
