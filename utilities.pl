:- use_module(library(lists)).
:- use_module(library(random)).

%/----------------------------------------/

% Predicate that checks if a list has repeated elements
% +List
has_repeated_elements(List) :-
    member(Element, List),
    select(Element, List, Rest),
    member(Element, Rest).

%/----------------------------------------/

%Predicate that checks if a number is even
% +X -> Number
isEven(X) :-
    mod(X,2) =:= 0.

%/----------------------------------------/

% Predicates that get the value from the coordinates in a 2 by 2 matrix
get_board_value_row([Value|_],0,Value).

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

% Predicates that replace the element from the coordinates in a 2 by 2 matrix with a specified value
replace_board_value_row([_|Tail],0,Value,[Value|Tail]).

replace_board_value_row([Head|Tail],Column,Value,[Head|NewTail]) :-
    Column > 0,
    Column1 is Column - 1,
    replace_board_value_row(Tail,Column1,Value,NewTail).

replace_board_value([Head|Tail],0,Column,Value,[NewHead|Tail]) :-
    replace_board_value_row(Head,Column,Value,NewHead).

replace_board_value([Head|Tail],Row,Column,Value,[Head|NewTail]) :-
    Row > 0,
    Row1 is Row - 1,
    replace_board_value(Tail,Row1,Column,Value,NewTail).

%/----------------------------------------/

% Predicates that translate a string or a char to an integer
string_to_int(String, Int) :-
    string_chars(String, Chars),
    maplist(char_to_int, Chars, Ints),
    foldl(+, Ints, 0, Int).

char_to_int(Char, Int) :-
    char_code(Char, Code),
    Int is Code - 48.

+(_, Acc, Acc).

%/----------------------------------------/

% Predicate that clears the console
clear_console :-
    write('\e[2J'),
    statistics(walltime, [T1|_]),
    repeat,
        statistics(walltime, [T2|_]),
        Delta is T2 - T1,
        Delta >= 500,
    !.

%/----------------------------------------/

% Predicate that gets the pair of coordinates from a piece predicate
piece_to_pair(piece(_, X, Y), [X, Y]).

%/----------------------------------------/

% Predicates that sort a list using the quicksort algorithm. Depending on the player,
% the list will be sorted in ascending order (player 1) or descending order (player 2) of
% the Y coordinates, in order to determine the best possible pieces (the closest ones to the enemy side)
% +Player
% +Coordinates
% -Sorted -> Sorted list of the coordinates
quicksort_pieces(_,[], []).
quicksort_pieces(1,[Head|Tail], Sorted) :-
    split_pieces(1,Head, Tail, Smaller, Larger),
    quicksort_pieces(1,Smaller, SortedSmaller),
    quicksort_pieces(1,Larger, SortedLarger),
    append(SortedSmaller, [Head|SortedLarger], Sorted).
    
quicksort_pieces(2,[Head|Tail], Sorted) :-
    split_pieces(2,Head, Tail, Larger, Smaller),
    quicksort_pieces(2,Larger, SortedLarger),
    quicksort_pieces(2,Smaller, SortedSmaller),
    append(SortedLarger, [Head|SortedSmaller], Sorted).

%/----------------------------------------/

% Auxiliary predicate used in the quick sort algorithm to split the list
% according to the player (ascending or descending order)
% +Player
% +Pivot
% +Coordinates
% -LeftCoordinates
% -RightCoordinates
split_pieces(_,_, [], [], []).
split_pieces(1,Pivot, [Head|Tail], [Head|Smaller], Larger) :-
    piece_to_pair(Head, Pair),
    piece_to_pair(Pivot, PivotPair),
    Pair = [_, Y],
    PivotPair = [_, PivotY],
    Y < PivotY,
    split_pieces(1,Pivot, Tail, Smaller, Larger).

split_pieces(1,Pivot, [Head|Tail], Smaller, [Head|Larger]) :-
    piece_to_pair(Head, Pair),
    piece_to_pair(Pivot, PivotPair),
    Pair = [_, Y],
    PivotPair = [_, PivotY],
    Y >= PivotY,
    split_pieces(1,Pivot, Tail, Smaller, Larger).

split_pieces(2,Pivot, [Head|Tail], [Head|Larger], Smaller) :-
    piece_to_pair(Head, Pair),
    piece_to_pair(Pivot, PivotPair),
    Pair = [_, Y],
    PivotPair = [_, PivotY],
    Y > PivotY,
    split_pieces(2,Pivot, Tail, Larger, Smaller).

split_pieces(2,Pivot, [Head|Tail], Larger, [Head|Smaller]) :-
    piece_to_pair(Head, Pair),
    piece_to_pair(Pivot, PivotPair),
    Pair = [_, Y],
    PivotPair = [_, PivotY],
    Y =< PivotY,
    split_pieces(2,Pivot, Tail, Larger, Smaller).
