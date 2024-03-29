:- consult('game.pl').

valid_menu_choice(1).
valid_menu_choice(2).
valid_menu_choice(3).

valid_play_menu_choice(1).
valid_play_menu_choice(2).
valid_play_menu_choice(3).
valid_play_menu_choice(4).

valid_level(1).
valid_level(2).

valid_player(1).
valid_player(2).

% Predicate that displays simple rules of the game
display_rules :-
    clear_console,
    write('\nTaacoca is a two-player abstract board game played on a hexagonal board with five cells per side.\n\nThe objective of Taacoca game is to reach the opponents Home row with one of the players pieces.\n\nA player also wins a game if the opponent is left without any pieces..\n\nStarting with player 1, players take turns moving any three of their pieces one cell forward. If a player has less than three pieces he must move all the remaning pieces, if thats not possible, he loses the game. The chosen pieces do not need to be connected to each other but they must move in the same direction.\nA player cannot move his pieces if one of the target cells is occupied with another piece of the player. If some of the target cells, or all of them, are occupied with opponents pieces then the opponents pieces are captured and removed from the board.'), nl.

%/----------------------------------------/

% Predicate that starts the player vs player game mode
start_pvp :-
    write('\nStarting player vs. player game...\n'),

    clear_console,
    game_pvp,!.

%/----------------------------------------/

% Predicate that starts the player vs AI game mode
% +Level -> level of difficulty
% +Player -> player chosen
start_pvai(Level,Player) :-
    write('\nStarting player vs. AI game...\n'),

    clear_console,
    game_pvai(Level,Player),!.

%/----------------------------------------/

% Predicate that starts the AI vs AI game mode
% +Level1 -> level of difficulty of AI #1
% +Level2 -> level of difficulty of AI #2
start_aivai(Level1,Level2) :-
    write('\nStarting AI vs. AI game...\n'),

    clear_console,
    game_aivai(Level1,Level2),!.

%/----------------------------------------/

% Predicates that represent valid menu options 
menu_choice(1) :-
    play_menu.

menu_choice(2) :-
    display_rules,
    fail.

menu_choice(3) :-
    write('\nGoodbye!\n'),
    write('\n=========================================================\n').

%/----------------------------------------/

% Predicate that lets the player vs player mode begin
play_menu_choice(1) :-
    start_pvp.

% Predicate where the player chooses the AI difficulty and which player they want to be, and that lets
% the player vs AI mode begin
play_menu_choice(2) :-
    repeat,
    write('\nWhat level of difficulty do you want to play in?\n'),
    write('1 - Easy // 2 - Hard\n'),

    get_char(LevelChar),
    char_to_int(LevelChar,Level),skip_line,
    valid_level(Level),

    repeat,
    write('\nWhich player do you want to be?\n'),
    write('1 - Player 1 // 2 - Player 2\n'),

    get_char(PlayerChar),
    char_to_int(PlayerChar,Player),skip_line,

    
    valid_player(Player),

    start_pvai(Level,Player).

% Predicate where the player chooses the AI difficulty and that lets the AI vs AI mode begin
play_menu_choice(3) :-
    repeat,
    write('\nWhat level of difficulty should AI #1 play in?\n'),
    write('1 - Easy // 2 - Hard\n'),

    get_char(LevelChar1),
    char_to_int(LevelChar1,Level1),skip_line,

    write('\nWhat level of difficulty should AI #2 play in?\n'),
    write('1 - Easy // 2 - Hard\n'),

    get_char(LevelChar2),
    char_to_int(LevelChar2,Level2),skip_line,

    valid_level(Level1),
    valid_level(Level2),

    start_aivai(Level1,Level2).

%/----------------------------------------/

% Predicate of the main menu of the game, where the player can choose to play the game, view the rules or quit
play :-
    repeat,
    write('\n=========================================================\n'),
    write('\nWelcome to Taacoca!\n'),
    write('\n1. Play\n'),
    write('2. Rules\n'),
    write('3. Exit\n'),

    repeat,
    write('\nPlease enter your choice: \n'),

    get_char(ChoiceChar),
    char_to_int(ChoiceChar,Choice),skip_line,
    valid_menu_choice(Choice),
    menu_choice(Choice).

%/----------------------------------------/

% Predicate where the player chooses the game mode they want to play
play_menu :-
    repeat,
    write('\n1. Player vs. Player\n'),
    write('2. Player vs. AI\n'),
    write('3. AI vs. AI\n'),
    repeat,
    write('\nPlease enter your choice: \n'),
    get_char(ChoiceChar),skip_line,
    char_to_int(ChoiceChar,Choice),
    valid_play_menu_choice(Choice),
    play_menu_choice(Choice).

