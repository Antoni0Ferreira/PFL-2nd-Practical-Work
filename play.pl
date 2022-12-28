:- consult('game.pl').

valid_menu_choice(1).
valid_menu_choice(2).
valid_menu_choice(3).

valid_play_menu_choice(1).
valid_play_menu_choice(2).
valid_play_menu_choice(3).
valid_play_menu_choice(4).

% Predicate that displays simple rules of the game
display_rules :-
    write('\nTaacoca is a two-player strategy game played on a grid. Each player takes turns placing their pieces on the grid, trying to get three in a row horizontally, vertically, or diagonally. The first player to get three in a row wins the game.'), nl.

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

    clear_console,skip_line,
    game_pvai(Level,Player),!.

%/----------------------------------------/

% Predicate that starts the AI vs AI game mode
% +Level -> level of difficulty
start_aivai(Level) :-
    write('\nStarting AI vs. AI game...\n'),

    clear_console,
    game_aivai(Level),!.

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
    write('\nWhat level of difficulty do you want to play in?\n'),
    write('1 - Easy // 2 - Hard\n'),

    get_char(LevelChar),
    char_to_int(LevelChar,Level),skip_line,

    write('\nWhich player do you want to be?\n'),
    write('1 - Player 1 // 2 - Player 2\n'),

    get_char(PlayerChar),
    char_to_int(PlayerChar,Player),

    start_pvai(Level,Player).

% Predicate where the player chooses the AI difficulty and that lets the AI vs AI mode begin
play_menu_choice(3) :-
    write('\nWhat level of difficulty do you want to play in?\n'),
    write('1 - Easy // 2 - Hard\n'),

    get_char(LevelChar),
    char_to_int(LevelChar,Level),
    start_aivai(Level).

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

