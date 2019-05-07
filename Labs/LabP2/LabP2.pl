% David Johansson

ins(X, [], [X]).
ins((Xseq, X, Xf, Xl), [(Yseq, Y, Yf, Yl)| Tail], [(Xseq, X, Xf, Xl), (Yseq, Y, Yf, Yl) | Tail]) :- X =< Y.
ins((Xseq, X, Xf, Xl), [(Yseq, Y, Yf, Yl)| Tail], [(Yseq, Y, Yf, Yl)|Tail1]) :- 
        X > Y, ins((Xseq, X, Xf, Xl), Tail, Tail1).

iSort([], []).
iSort([X| Tail], Sorted) :- 
    iSort(Tail, TailS),
    ins(X, TailS, Sorted).

conc([], L, L).
conc([X| L1], L2, [X| L3]) :- conc(L1, L2, L3).

take(1, [H|_], [H]).
take(K, [H|T], [H|T2]) :- K > 1, K2 is K-1, take(K2, T, T2).

sum([X], X).
sum([X, Y | T], Tot) :- Sum is X+Y, sum([Sum|T], Tot).

len([], 0).
len([_|T], L) :- len(T, L2), L is L2+1.

takeKtoOne(S, 1, [X|_], [([X], X, S, S)]).
takeKtoOne(S, K, L, [(Seq, Sum, S, E) | Seqs]) :- 
    K > 1, take(K, L, Seq), 
    sum(Seq, Sum), K2 is K-1, 
    E is S+K2, takeKtoOne(S, K2, L, Seqs).

allSubSeq(S, [X], [([X], X, S, S)]).
allSubSeq(S, [H|T], All) :- 
    len([H|T], Len),
    takeKtoOne(S, Len, [H|T], L1),
    S2 is S+1,
    allSubSeq(S2, T, L2),
    conc(L1, L2, All).

wLines(_, []).
wLines(0, _).
wLines(K, [(Seq, S, F, L) | Rest]) :-
    K > 0,
    write(S), write('     '), write(F), write('    '), 
    write(L), write('    '), write(Seq), nl,
    K2 is K-1, wLines(K2, Rest).

% Main function
sks(K, L) :- 
    write('Entire list: '), write(L), nl,
    write('size    i    j    sublist'), nl,
    allSubSeq(1, L, Seq),
    iSort(Seq, SeqSort),
    wLines(K, SeqSort).

/* Testcases
sks(6, [24,-11,-34,42,-24,7,-19,21]).
sks(8, [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]).
*/