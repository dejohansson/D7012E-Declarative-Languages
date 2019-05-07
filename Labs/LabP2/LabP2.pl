% David Johansson

ins(X, [], [X]).
ins(X, [Y| Tail], [X, Y | Tail]) :- X =< Y.
ins(X, [Y| Tail], [Y|Tail1]) :- X > Y, ins(X, Tail, Tail1).

iSort([], []).
iSort([X| Tail], Sorted) :- 
    iSort(Tail, TailS),
    ins(X, TailS, Sorted).