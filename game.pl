:- consult('game_logic.pl').
:- consult('game_logic_computer.pl').

% Predicate used for the user's turn. The user will choose which N pieces and in what direction
% they will move.
% +Gamestate -> Current board
% +Player
% -NewGamestate -> New Board
move(Gamestate,Player,NewGamestate) :-

    format('\nPLAYER ~d -\n',[Player]),

    get_direction(Dir),
    get_number_plays(Player,N),
    choose_pieces(Gamestate,Player,Dir,[],NewPieces,NewGamestate1,N),
    move_pieces(NewGamestate1,Player,NewPieces,NewGamestate,N),
    check_attacks(Player,NewPieces,N),

    write('---------------------------------------------------------\n'),skip_line,!.

%/----------------------------------------/

% Predicate used by the AI (easy difficulty) during its turn to choose the pieces it
% wants to move and move the pieces. It should be noted that, in this difficulty, since the pieces
% are randomly chosen, the process of choosing will be repeated until N pieces that can move
% to valid spaces are chosen.
% +Gamestate -> Current board
% +Player
% +1 - Level of difficulty (easy)
% -NewGamestate -> New Board
choose_move(Gamestate,Player,1,NewGamestate) :-

    format('\nCOMPUTER - PLAYER ~d\n',[Player]),

    get_direction_computer(Dir),
    get_number_plays(Player,N),
    choose_pieces_computer(Gamestate,Player,Dir,[],NewPieces,NewGamestate1,N),
    move_pieces(NewGamestate1,Player,NewPieces,NewGamestate,N),
    check_attacks(Player,NewPieces,N),

    write('---------------------------------------------------------\n'),!.

% Predicate used by the AI (hard difficulty), during its turn, to choose the best pieces it
% wants to move, determine the best possible moves and move the pieces
% +Gamestate -> Current board
% +Player
% +2 - Level of difficulty (hard)
% -NewGamestate -> New Board
choose_move(Gamestate,Player,2,NewGamestate) :-

    format('\nCOMPUTER - PLAYER ~d\n',[Player]),

    get_number_plays(Player,N),
    choose_best_pieces(Player,ChosenPieces,N),
    get_valid_moves(Gamestate,Player,ChosenPieces,[],NewPieces,N),
    find_next_pieces(Gamestate,Player,NewPieces,BestPieces,N),
    clean_game(Gamestate,Player,ChosenPieces,NewGamestate1,N),
    move_pieces(NewGamestate1,Player,BestPieces,NewGamestate,N),
    check_attacks(Player,BestPieces,N),

    write('---------------------------------------------------------\n'),!.

%/----------------------------------------/

% Predicate that checks if the moves are all valid
% +Gamestate -> Current board
% Player
% +[[X,Y]|Tail] -> List of the new coordinates of the chosen pieces
% ?N -> Number of iterations
check_valid_spaces(Gamestate,Player,[Head|Tail],N) :-

    N > 0,
    check_valid_space(Gamestate,Player,Head),
    N1 is N - 1,
    check_valid_spaces(Gamestate,Player,Tail,N1).

check_valid_spaces(_,_,[],0).

%/----------------------------------------/

% Predicate that checks if the movement of the pieces results in any attacks
% Player
% +[[X,Y]|Tail] -> List of the new coordinates of the chosen pieces
% ?N -> Number of iterations
check_attacks(Player,[[X,Y]|Tail],N) :-

    N > 0,
    check_attack(Player,[X,Y]),
    N1 is N - 1,
    check_attacks(Player,Tail,N1).

check_attacks(_,_,0).

%/----------------------------------------/

% Predicate where the selected spaces will be cleaned, in order to prepare for
% the movement of the pieces
% +Gamestate -> Current board
% Player
% +[[X,Y]|Tail] -> List of the coordinates of the chosen pieces
% -NewGamestate -> New Board
% ?N -> Number of iterations
clean_game(Gamestate,Player,[[X,Y]|Tail],NewGamestate,N) :-

    N > 0,
    clean_space(Gamestate,Player,[X,Y],NewGamestate1),
    N1 is N - 1,
    clean_game(NewGamestate1,Player,Tail,NewGamestate,N1).

clean_game(Gamestate,_,[],Gamestate,0).

%/----------------------------------------/

% Predicate where the selected spaces will be emptied to facilitate
% the validation of the moves
% +Gamestate -> Current board
% +[[X,Y]|Tail] -> List of the coordinates of the chosen pieces
% -NewGamestate -> New Board
% ?N -> Number of iterations  
prepare_move(Gamestate,[[X,Y]|Tail],NewGamestate,N) :-

    N > 0,
    prepare_space(Gamestate,[X,Y],NewGamestate1),
    N1 is N - 1,
    prepare_move(NewGamestate1,Tail,NewGamestate,N1).

prepare_move(Gamestate,[],Gamestate,0).

%/----------------------------------------/

% Predicate where the pieces will move to their new coordinates
% +Gamestate -> Current board
% +Player
% +Dir -> Direction
% +[[X,Y]|Tail] -> List of the coordinates of the new spaces
% -NewGamestate -> New Board
% ?N -> Number of iterations
move_pieces(Gamestate,Player,[[X,Y]|Tail],NewGamestate,N) :-

    N > 0,
    move_piece(Gamestate,Player,[X,Y],NewGamestate1),
    N1 is N - 1,
    move_pieces(NewGamestate1,Player,Tail,NewGamestate,N1).

move_pieces(Gamestate,_,_,Gamestate,0).

%/----------------------------------------/

% Predicate where the user will chosoe the pieces to move,
% get the coordinates of the spaces where the chosen pieces will move, prepare the move,
% check if the coordinates obtained are valid and clean the spaces of initial pieces chosen
% +Gamestate -> Current board
% +Player
% +Dir -> Direction
% +Pieces -> List of the coordinates of the chosen pieces
% -NewPieces -> List of the new coordinates 
% -NewGamestate -> New Board
% ?N -> Number of iterations
choose_pieces(Gamestate,Player,Dir,Pieces,NewPieces,NewGamestate,N) :-
    repeat,
    choose_pieces_rec(Gamestate,Player,Pieces,ChosenPieces,N),
    \+has_repeated_elements(ChosenPieces),

    get_new_spaces(Player,ChosenPieces,Dir,[],NewPieces,N),

    prepare_move(Gamestate,ChosenPieces,NewGamestate1,N),
    check_valid_spaces(NewGamestate1,Player,NewPieces,N),
    clean_game(Gamestate,Player,ChosenPieces,NewGamestate,N).

%/----------------------------------------/

% Predicate where the AI (easy difficulty) will choose the pieces to move,
% get the coordinates of the spaces where the chosen pieces will move, prepare the move,
% check if the coordinates obtained are valid and clean the spaces of initial pieces chosen
% +Gamestate -> Current board
% +Player
% +Dir -> Direction
% +Pieces -> List of the coordinates of the chosen pieces
% -NewPieces -> List of the new coordinates 
% -NewGamestate -> New Board
% ?N -> Number of iterations
choose_pieces_computer(Gamestate,Player,Dir,Pieces,NewPieces,NewGamestate,N) :-
    repeat,
    choose_pieces_computer_rec(Gamestate,Player,Pieces,ChosenPieces,N),
    \+has_repeated_elements(ChosenPieces),

    get_new_spaces(Player,ChosenPieces,Dir,[],NewPieces,N),

    prepare_move(Gamestate,ChosenPieces,NewGamestate1,N),
    check_valid_spaces(NewGamestate1,Player,NewPieces,N),
    clean_game(Gamestate,Player,ChosenPieces,NewGamestate,N).

%/----------------------------------------/

% Predicate that obtains the coordinates of the next spaces
% to where the pieces will move, according to the direction chosen
% +Player
% +[[X,Y]|Tail] -> List of the coordinates of the chosen pieces 
% +Dir -> Direction
% ?Acc -> Accumulator
% -NewPieces -> List of the new coordinates
% ?N -> Number of iterations 
get_new_spaces(Player,[[X,Y]|Tail],Dir,Acc,NewPieces,N) :-

    N > 0,
    get_new_space(Player,[X,Y],Dir,[X1,Y1]),
    N1 is N - 1,
    get_new_spaces(Player,Tail,Dir,[[X1,Y1]|Acc],NewPieces,N1).

get_new_spaces(_,[],_,Pieces,Pieces,0).

%/----------------------------------------/

% Predicate that obtains all the possible valid next moves of the
% chosen pieces, in the AI vs AI mode.
% +Gamestate -> Current board
% +Player
% +[[X,Y]|Tail] -> List of the coordinates of the chosen pieces 
% ?Acc -> Accumulator of all the valid moves
% -AllValidMoves -> List with all valid possibe next moves, for each chosen piece
% ?N -> Number of iterations
get_valid_moves(Gamestate,Player,[[X,Y]|Tail],Acc,AllValidMoves,N) :-

    N > 0,
    valid_moves(Gamestate,Player,[X,Y],ValidMoves),
    N1 is N - 1,
    get_valid_moves(Gamestate,Player,Tail,[ValidMoves|Acc],AllValidMoves,N1).

get_valid_moves(_,_,[],ValidMoves,ValidMoves,0).

%/----------------------------------------/

% Predicate that determines that the player should choose the next left
% pieces in the AI vs AI mode. In this case, the weight of the right pieces is
% bigger/equal than the weight of the right pieces. So, the left pieces are chosen.
% +Gamestate -> Current board
% +Player
% +[Head|Tail] -> List of possible next pieces
% -NewPieces -> List of chosen next pieces
% ?N -> Number of iterations
find_next_pieces(Gamestate,Player,[Head|Tail],NewPieces,N) :-

    find_direction([Head|Tail],0,0,Left,Right),
    Left >= Right,
    choose_left_pieces([Head|Tail],[],NewPieces),
    check_valid_spaces(Gamestate,Player,NewPieces,N),
    retractall(next_piece(_,_,_)).

% Predicate that determines that the player should choose the next right
% pieces in the AI vs AI mode. In this case, the weight of the right pieces is
% bigger than the weight of the left pieces. So, the right pieces are chosen.
% +Gamestate -> Current board
% +Player
% +[Head|Tail] -> list of possible next pieces
% -NewPieces -> list of chosen next pieces
% ?N -> Number of iterations
find_next_pieces(Gamestate,Player,[Head|Tail],NewPieces,N) :-

    find_direction([Head|Tail],0,0,Left,Right),
    Left < Right,
    choose_right_pieces([Head|Tail],[],NewPieces),
    check_valid_spaces(Gamestate,Player,NewPieces,N),
    retractall(next_piece(_,_,_)).

%/----------------------------------------/

% Predicate of the player vs player game mode loop (player 1 won)
game(FinalGamestate,N,FinalGamestate) :-
    isEven(N),
    game_over(1),
    write('\nCongrats, player 1 won!\n').

% Predicate of the player vs player game mode loop (player 2 won)
game(FinalGamestate,N,FinalGamestate) :-
    \+isEven(N),
    game_over(2),

    write('\nCongrats, player 2 won!\n').

% Predicate of the player vs player game mode loop (player 2's turn)
% +Gamestate -> Current board
% +N -> Iteration of the loop
% -FinalGamestate -> final state of the board
game(Gamestate,N,FinalGamestate) :-
    isEven(N),
    \+game_over(1),
    \+game_over(2),
    display_game(Gamestate),
    move(Gamestate,2,NewGamestate),
    N1 is N + 1,
    game(NewGamestate,N1,FinalGamestate).    

% Predicate of the player vs player game mode loop (player 1's turn)
% +Gamestate -> Current board
% +N -> Iteration of the loop
% -FinalGamestate -> final state of the board
game(Gamestate,N,FinalGamestate) :-
    \+isEven(N),
    \+game_over(1),
    \+game_over(2),
    display_game(Gamestate),
    move(Gamestate,1,NewGamestate),
    N1 is N + 1,
    game(NewGamestate,N1,FinalGamestate).

%/----------------------------------------/

% Predicate that indicates that the game can end, since, after their move, 
% player (AI) won
next_move_one_computer(FinalGamestate,2,_,FinalGamestate) :-
    game_over(1),

    write('\nSorry, you lost :(\n').

% Predicate that lets the second player move, since player 1 didn't win
% after their move
% +Gamestate -> Current board
% +Player -> player chosen by the user (2)
% +Level -> level of difficulty
% -FinalGamestate -> final state of the board
next_move_one_computer(Gamestate,2,Level,FinalGamestate) :-
    \+game_over(1),
    display_game(Gamestate),
    move(Gamestate,2,NewGamestate),
    game_one_computer(NewGamestate,2,Level,FinalGamestate).

% Predicate that indicates that the game can end, since, after their move,
% player 1 (user) won
next_move_one_computer(FinalGamestate,1,_,FinalGamestate) :-
    game_over(1),

    write('\nCongrats, you won!\n').

% Predicate that lets the second player move, since player 1 didn't win
% after their move
% +Gamestate -> Current board
% +Player -> player chosen by the user (1)
% +Level -> level of difficulty
% -FinalGamestate -> final state of the board
next_move_one_computer(Gamestate,1,Level,FinalGamestate) :-
    \+game_over(1),
    display_game(Gamestate),
    choose_move(Gamestate,2,Level,NewGamestate),
    game_one_computer(NewGamestate,1,Level,FinalGamestate).

%/----------------------------------------/

% Predicate that indicates that user won
game_one_computer(FinalGamestate,1,_,FinalGamestate) :-
    game_over(1),

    write('\nCongrats, you won!\n').

% Predicate that indicates that the AI won
game_one_computer(FinalGamestate,1,_,FinalGamestate) :-
    game_over(2),
    
    write('\nSorry, you lost :(\n').

% Predicate of the player vs AI game mode loop, where player 1 is the user. In this, the AI plays
% its move and the game checks if they computer won or if it can continue
% +Gamestate -> Current board
% +Player -> player chosen by the user (1)
% +Level -> level of difficulty
% -FinalGamestate -> final state of the board
game_one_computer(Gamestate,1,Level,FinalGamestate) :-
    \+game_over(1),
    \+game_over(2),
    display_game(Gamestate),
    move(Gamestate,1,NewGamestate),
    next_move_one_computer(NewGamestate,1,Level,FinalGamestate).

% ----//----
% Predicate that indicates that the AI won
game_one_computer(FinalGamestate,2,_,FinalGamestate) :-
    game_over(1),

    write('\nSorry, you lost :(\n').

% Predicate that indicates that user won
game_one_computer(FinalGamestate,2,_,FinalGamestate) :-
    game_over(2),

    write('\nCongrats, you won!\n').

% Predicate of the player vs AI game mode loop, where player 1 is the AI. In this, the AI plays
% its move and the game checks if they computer won or if it can continue
% +Gamestate -> Current board
% +Player -> player chosen by the user (2)
% +Level -> level of difficulty
% -FinalGamestate -> final state of the board
game_one_computer(Gamestate,2,Level,FinalGamestate) :-
    \+game_over(1),
    \+game_over(2),
    display_game(Gamestate),
    choose_move(Gamestate,1,Level,NewGamestate),
    next_move_one_computer(NewGamestate,2,Level,FinalGamestate).

%/----------------------------------------/

% Predicate that indicates that the game can end, since, after their move, player 1 won
next_move_two_computers(FinalGamestate,_,FinalGamestate) :-
    game_over(1),

    write('\nCongrats, Computer #1 won!\n').

% Predicate that lets the second player move, since player 1 didn't win
% after their move
% +Gamestate -> Current board
% +Level -> level of difficulty
% -FinalGamestate -> final state of the board
next_move_two_computers(Gamestate,Level,FinalGamestate) :-
    \+game_over(1),
    display_game(Gamestate),
    choose_move(Gamestate,2,Level,NewGamestate),
    game_two_computers(NewGamestate,Level,FinalGamestate).

%/----------------------------------------/

% Predicate that indicates that player 1 won
game_two_computers(FinalGamestate,_,FinalGamestate) :-
    game_over(1),

    write('\nCongrats, Computer #1 won!\n').

% Predicate that indicates that player 2 won
game_two_computers(FinalGamestate,_,FinalGamestate) :-
    game_over(2),

    write('\nCongrats, Computer #2 won!\n').

% Predicate of the AI vs AI game mode loop, where player 1 plays their move and
% the game checks if they won or not
% +Gamestate -> Current board
% +Level -> level of difficulty
% -FinalGamestate -> final state of the board
game_two_computers(Gamestate,Level,FinalGamestate) :-
    \+game_over(1),
    \+game_over(2),
    display_game(Gamestate),
    choose_move(Gamestate,1,Level,NewGamestate),
    next_move_two_computers(NewGamestate,Level,FinalGamestate).

%/----------------------------------------/

% Predicates that represent the available game modes. They all create the board, start the game
% and display the final state of the board.

% Player vs Player game mode
game_pvp :-
    write('\n=========================================================\n'),

    empty_board(InitialGamestate),
    game(InitialGamestate,1,FinalGamestate),
    display_game(FinalGamestate),

    write('\n=========================================================\n').

% Player vs AI game mode
% +Level -> level of difficulty
% +Player -> player chosen
game_pvai(Level,Player) :-
    write('\n=========================================================\n'),

    empty_board(InitialGamestate),
    game_one_computer(InitialGamestate,Player,Level,FinalGamestate),
    display_game(FinalGamestate),

    write('\n=========================================================\n').

% AI vs AI game mode
% +Level -> level of difficulty
game_aivai(Level) :-
    write('\n=========================================================\n'),

    empty_board(InitialGamestate),
    game_two_computers(InitialGamestate, Level, FinalGamestate),
    display_game(FinalGamestate),

    write('\n=========================================================\n').
