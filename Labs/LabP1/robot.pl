move(state(KeyB, KeyS, Package, Robot), NewState, Action) :-
    Robot = r1,
    KeyS = r1,
    NewState = state(KeyB, r2, Package, r2),
    Action = [keyB, nothing, r2].

%state(KeyB, KeyS, Package, Robot)
solveR(state(_, _, r2, _), _, []). % Package in room 2.
solveR(State, N, Action) :-
    N>0,
    move(State, NewState, [Key, Item, Room]),
    solveR(NewState, N-1, Actions),
    Action = [move(Key, Item, Room) | Actions].

start(Action) :- solveR(state(r2, r1, r3, r1), 5, Action).