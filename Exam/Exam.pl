/*factorial(0, 1).
factorial(N, NF) :- 
    N>0,
    N2 is N-1,
    factorial(N2, NF2), 
    NF is N*NF2, !.

factorials(N,L) :-
    N >= 0,
    factorial(N,FN),
    N2 is N-1,
    factorials(N2,List),
    L2 = [FN | List],
    sort(0, @=<, L2, L), !.
factorials(_,[]) :- !.

iT(1).

p(A, B) :- iT(A), iT(B), !.
p(A, B) :- iT(B).

P :- A, !, B.
P :- B.

P :- A, B, !.
P :- B.

siblings(X, Y) :-
    parent(P, X), parent(P, Y).

cousins(X, Y) :-
    parent(PX, X), 
    parent(PY, Y),
    siblings(PX, PY).

f(_, [], 0).
f(X, [X | L], N) :-
    f(X, L, NTemp),
    N is 1+NTemp, !.
f(X, [_ | L], N) :-
    f(X, L, N).

freqList(L,FL) :- setof([S,D],(member(S,L),f(S,L,D)),FL).*/

/*fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, NF) :-
    N>1,
    N2 is N-1, N3 is N-2,
    fib(N2, R1), fib(N3, R2),
    NF is R1+R2.

fibs(N, L) :-
    N >= 0,
    fib(N, NF),
    N1 is N-1,
    fibs(N1, L2),
    %L3 = [NF | L2],
    sort(0, @=<, [NF | L2], L), !.
fibs(_, []) :- !.

arc(n1,n2).
arc(n6,n2).
arc(n2,n5).
arc(n2,n3).
arc(n3,n7).
arc(n3,n4).

path(A, B, [arc(A, B)]) :- arc(A, B), !.
path(A, B, [arc(A, X) | P]) :-
    arc(A, X),
    path(X, B, P), !.

add(arc(A, A)) :- !, fail.
add(arc(A, B)) :-
    \+ path(B, A, _),
    asserta(arc(A, B)), !. 

allPaths(N, L) :-
    N2 is N-1,
    setof(X, A^B^(path(A,B,X),length(X,N2)), L).*/

/*select(_, [], _) :- fail.
select(X, [X|L], L).
select(X, [Y|L], [Y|L2]) :-
    select(X, L, L2).

pickTwoDifferent([],_,_) :- fail.
pickTwoDifferent([_|[]],_,_) :- fail.
pickTwoDifferent(L,E1,E2) :- select(E1, L, L2), select(E2, L2, _).

subLists(L,R) :- setof(X,combinations(L,X),R).

combinations([],[]).
combinations([V|Vs],[V|S]) :- combinations(Vs, S).
combinations([_|Vs],S) :- combinations(Vs, S).

sum([], 0).
sum([X|L], S) :- sum(L, S2), S is X+S2.

keep(L,Max,R) :-
    findall(X, (member(X, L), sum(X, S), S =< Max), R).*/

append2([],L2,L2).
append2([H|T],L2,[H|L4]) :- append2(T,L2,L4).

concat2([],[]).
concat2([H|T],L) :- concat2(T,L1), append2(H,L1,L).