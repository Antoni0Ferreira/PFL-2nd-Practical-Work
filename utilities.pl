last_element([Head],Head).
last_element([_|Tail],X) :-
    last_element(Tail,X).

%/----------------------------------------/

get_board_value_row([Head|_],0,Value) :-
    Value = Head.

get_board_value_row([Head|Tail],Column,Value) :-
    Column > 0,
    Column1 is Column - 1,
    get_board_value_row(Tail,Column1,Value).

get_board_value([Head|Tail], 0, Column, Value) :-
    get_board_value_row(Head,Column,Value).

get_board_value([Head|Tail], Row, Column, Value) :-
    Row > 0,
    Row1 is Row - 1,
    get_board_value(Tail, Row1, Column, Value).

%/----------------------------------------/

replace_board_value_row([_Head|Tail],0,Value,[Value|Tail]).

replace_board_value_row([Head|Tail],Column,Value,[Head|New_tail]) :-
    Column > 0,
    Column1 is Column - 1,
    replace_board_value_row(Tail,Column1,Value,New_tail).

replace_board_value([Head|Tail],0,Column,Value,[New_head|Tail]) :-
    replace_board_value_row(Head,Column,Value,New_head).

replace_board_value([Head|Tail],Row,Column,Value,[Head|New_tail]) :-
    Row > 0,
    Row1 is Row - 1,
    replace_board_value(Tail,Row1,Value,New_tail).