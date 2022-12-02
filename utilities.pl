lastElement([Head],Head).
lastElement([_|Tail],X) :-
    lastElement(Tail,X).