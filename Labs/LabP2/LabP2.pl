% David Johansson

ins(X, [], [X]).
ins(X, [Y| Tail], [X, Y | Tail]) :- X =< Y.
ins(X, [Y| Tail], [Y|Tail1]) :- X > Y, ins(X, Tail, Tail1).

iSort([], []).
iSort([X| Tail], Sorted) :- 
    iSort(Tail, TailS),
    ins(X, TailS, Sorted).

take(1, [H|_], [H]).
take(K, [H|T], [H|T2]) :- K > 1, K2 is K-1, take(K2, T, T2).

sum([X], X).
sum([X, Y | T], Tot) :- Sum is X+Y, sum([Sum|T], Tot).

takeKtoOne(S, 1, [X|_], [([X], X, S, S)]).
takeKtoOne(S, K, L, [(Seq, Sum, S, E) | Seqs]) :- 
    K > 1, take(K, L, Seq), 
    sum(Seq, Sum), K2 is K-1, 
    E is S+K2, takeKtoOne(S, K2, L, Seqs).

 