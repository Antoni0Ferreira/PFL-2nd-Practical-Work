translate_symbol(player_1,Symbol) :- Symbol = '+'.
translate_symbol(player_2,Symbol) :- Symbol = 'o'.

translate_number(0,Num) :- Num=' '.
translate_number(1,Num) :- Num='1'.
translate_number(2,Num) :- Num='2'.
translate_number(3,Num) :- Num='3'.
translate_number(4,Num) :- Num='4'.
translate_number(5,Num) :- Num='5'.
translate_number(6,Num) :- Num='6'.
translate_number(7,Num) :- Num='7'.
translate_number(8,Num) :- Num='8'.
translate_number(9,Num) :- Num='9'.

translate_letter('A',Num) :- Num=1.
translate_letter('B',Num) :- Num=2.
translate_letter('C',Num) :- Num=3.
translate_letter('D',Num) :- Num=4.
translate_letter('E',Num) :- Num=5.
translate_letter('F',Num) :- Num=6.
translate_letter('G',Num) :- Num=7.
translate_letter('H',Num) :- Num=8.
translate_letter('I',Num) :- Num=9.
translate_letter('J',Num) :- Num=10.
translate_letter('K',Num) :- Num=11.
translate_letter('L',Num) :- Num=12.
translate_letter('M',Num) :- Num=13.
translate_letter('N',Num) :- Num=14.
translate_letter('O',Num) :- Num=15.
translate_letter('P',Num) :- Num=16.
translate_letter('Q',Num) :- Num=17.

translate_letter('a',Num) :- Num=1.
translate_letter('b',Num) :- Num=2.
translate_letter('c',Num) :- Num=3.
translate_letter('d',Num) :- Num=4.
translate_letter('e',Num) :- Num=5.
translate_letter('f',Num) :- Num=6.
translate_letter('g',Num) :- Num=7.
translate_letter('h',Num) :- Num=8.
translate_letter('i',Num) :- Num=9.
translate_letter('j',Num) :- Num=10.
translate_letter('k',Num) :- Num=11.
translate_letter('l',Num) :- Num=12.
translate_letter('m',Num) :- Num=13.
translate_letter('n',Num) :- Num=14.
translate_letter('o',Num) :- Num=15.
translate_letter('p',Num) :- Num=16.
translate_letter('q',Num) :- Num=17.

%/----------------------------------------/

% Predicate that gets the initial board of the game
% -Board
initial_state([
[' ',' ',' ',' ',' ','_','_','_','_','_','_','_','_','_',' ',' ',' ',' ',' '],
[' ',' ',' ',' ','|','o','|','o','|','o','|','o','|','o','|',' ',' ',' ',' '],
[' ',' ',' ','|','_','|','o','|','o','|','o','|','o','|','_','|',' ',' ',' '],
[' ',' ','|','_','|','_','|','o','|','o','|','o','|','_','|','_','|',' ',' '],
[' ','|','_','|','_','|','_','|','_','|','_','|','_','|','_','|','_','|',' '],
['|','_','|','_','|','_','|','_','|','_','|','_','|','_','|','_','|','_','|'],
[' ','|','_','|','_','|','_','|','_','|','_','|','_','|','_','|','_','|',' '],
[' ',' ','|','_','|','_','|','+','|','+','|','+','|','_','|','_','|',' ',' '],
[' ',' ',' ','|','_','|','+','|','+','|','+','|','+','|','_','|',' ',' ',' '],
[' ',' ',' ',' ','|','+','|','+','|','+','|','+','|','+','|',' ',' ',' ',' ']
]).

%/----------------------------------------/

% Predicates used to display the board of the game
display_row([]).

display_row([Head|Tail]) :-
    write(Head),
    display_row(Tail).

display_board_matrix(_,10).
display_board_matrix([Head|Tail],N) :-
    translate_number(N,Num),
    write(Num),
    write(' '),
    display_row(Head),
    N1 is N + 1,
    nl,
    display_board_matrix(Tail, N1).

display_game(GameState) :-
    nl,
    write('   ABCDEFGHIJKLMNOPQ'),
    nl,
    display_board_matrix(GameState,0),
    nl,
    write('   ABCDEFGHIJKLMNOPQ'),
    nl,nl.

