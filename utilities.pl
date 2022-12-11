last_element([Head],Head).
last_element([_|Tail],X) :-
    last_element(Tail,X).

first_element([Head|_],Head).

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
    write('\ndentro do replace_board_value_row\n'),
    Column > 0,
    Column1 is Column - 1,
    replace_board_value_row(Tail,Column1,Value,NewTail).

replace_board_value([Head|Tail],0,Column,Value,[NewHead|Tail]) :-
    write('\ndentro do replace_board_value com row a 0\n'),
    write(Column),nl,
    replace_board_value_row(Head,Column,Value,NewHead).

replace_board_value([Head|Tail],Row,Column,Value,[Head|NewTail]) :-
    write('\ndentro do replace_board_value\n'),
    Row > 0,
    Row1 is Row - 1,
    replace_board_value(Tail,Row1,Column,Value,NewTail).