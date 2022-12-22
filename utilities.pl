:- use_module(library(lists)).
:- use_module(library(random)).

last_element([Head],Head).
last_element([_|Tail],X) :-
    last_element(Tail,X).

first_element([Head|_],Head).

%/----------------------------------------/

has_repeated_elements(List) :-
    member(Element, List),
    select(Element, List, Rest),
    member(Element, Rest).

%/----------------------------------------/

isEven(X) :-
    mod(X,2) =:= 0.

%/----------------------------------------/

get_board_value_row([Head|_],0,Value) :-
    Value = Head.

get_board_value_row([_|Tail],Column,Value) :-
    Column > 0,
    Column1 is Column - 1,
    get_board_value_row(Tail,Column1,Value).

get_board_value([Head|_], 0, Column, Value) :-
    get_board_value_row(Head,Column,Value).

get_board_value([_|Tail], Row, Column, Value) :-
    Row > 0,
    Row1 is Row - 1,
    get_board_value(Tail, Row1, Column, Value).

%/----------------------------------------/

replace_board_value_row([_|Tail],0,Value,[Value|Tail]).

replace_board_value_row([Head|Tail],Column,Value,[Head|NewTail]) :-
    % write('\ndentro do replace_board_value_row\n'),
    Column > 0,
    Column1 is Column - 1,
    replace_board_value_row(Tail,Column1,Value,NewTail).

replace_board_value([Head|Tail],0,Column,Value,[NewHead|Tail]) :-
    % write('\ndentro do replace_board_value com row a 0\n'),
    % write(Column),nl,
    replace_board_value_row(Head,Column,Value,NewHead).

replace_board_value([Head|Tail],Row,Column,Value,[Head|NewTail]) :-
    % write('\ndentro do replace_board_value\n'),
    Row > 0,
    Row1 is Row - 1,
    replace_board_value(Tail,Row1,Column,Value,NewTail).

%/----------------------------------------/

string_to_int(String, Int) :-
    string_chars(String, Chars),
    maplist(char_to_int, Chars, Ints),
    foldl(+, Ints, 0, Int).

char_to_int(Char, Int) :-
    char_code(Char, Code),
    Int is Code - 48.

+(_, Acc, Acc).


clear_console :-
    write('\e[2J'),
    statistics(walltime, [T1|_]),
    repeat,
        statistics(walltime, [T2|_]),
        Delta is T2 - T1,
        Delta >= 500,
    !.