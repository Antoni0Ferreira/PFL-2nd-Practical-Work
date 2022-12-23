vertex(a).
vertex(b).
vertex(c).
vertex(d).

edge(a, b).
edge(b, c).
edge(c, d).

% a -- b -- c -- d

neighbors(X, Y) :-
    edge(X, Y).
neighbors(X, Y) :-
    edge(Y, X).


shortest_path(Start, End, Path) :-
shortest_path(Start, End, [Start], Path).

shortest_path(End, End, Visited, Visited).
shortest_path(Current, End, Visited, Path) :-
    neighbors(Current, Next),
    \+ member(Next, Visited),
    shortest_path(Next, End, [Next|Visited], Path).