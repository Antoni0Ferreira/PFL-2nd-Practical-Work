:- consult('game_logic.pl').

get_direction_computer(Dir) :-
    DirList = ['l','r'],
    random_member(Dir,DirList).

choose_pieces_computer_rec(Gamestate,Player,Pieces,ChosenPieces,N) :-
    N > 0,
    % write('\ndentro do choose_pieces_computer_rec\n'),
    list_pieces(Player,PossiblePieces),
    % format('\nPossiblePieces - ~w\n',[PossiblePieces]),
    length(PossiblePieces,Length),
    % format('\nlength - ~d',Length),
    random(0,Length,Index),
    % format('\nindex - ~d\n',[Index]),
    nth0(Index,PossiblePieces,piece(Player,X,Y)),
    % format('\n[X,Y] - ~w\n',piece(Player,X,Y)),
    N1 is N - 1,

    choose_pieces_computer_rec(Gamestate,Player,[[X,Y]|Pieces],ChosenPieces,N1).

choose_pieces_computer_rec(_,_,ChosenPieces,ChosenPieces,0).

% valid_moves(Gamestate,Player,Dir).