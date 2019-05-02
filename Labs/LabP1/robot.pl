% David Johansson

moveItem(state(KeyB, KeyS, Room, Room), NewRoom, NewState, package) :-
    NewState = state(KeyB, KeyS, NewRoom, NewRoom).

moveItem(state(KeyB, Room, Package, Room), NewRoom, NewState, keyS) :-
    NewState = state(KeyB, NewRoom, Package, NewRoom).

moveItem(state(Room, KeyS, Package, Room), NewRoom, NewState, keyB) :-
    NewState = state(NewRoom, KeyS, Package, NewRoom).

moveItem(state(KeyB, KeyS, Package, _), NewRoom, NewState, nothing) :-
    NewState = state(KeyB, KeyS, Package, NewRoom).

move(state(KeyB, r1, Package, r1), NewState, Action) :-
    moveItem(state(KeyB, r2, Package, r1), r2, State, Item),
    NewState = State,
    Action = move(keyS, Item, r2).

move(state(r1, KeyS, Package, r1), NewState, Action) :-
    moveItem(state(r3, KeyS, Package, r1), r3, State, Item),
    NewState = State,
    Action = move(keyB, Item, r3).

move(state(KeyB, r2, Package, r2), NewState, Action) :-
    moveItem(state(KeyB, r1, Package, r2), r1, State, Item),
    NewState = State,
    Action = move(keyS, Item, r1).

move(state(r3, KeyS, Package, r3), NewState, Action) :-
    moveItem(state(r1, KeyS, Package, r3), r1, State, Item),
    NewState = State,
    Action = move(keyB, Item, r1).

%state(KeyB, KeyS, Package, Robot)
solveR(state(_, _, r2, _), _, []). % Package in room 2.
solveR(State, N, Action) :-
    N>0,
    move(State, NewState, NewAction),
    solveR(NewState, N-1, Trace),
    Action = [NewAction | Trace].

% Test
start(Action) :- solveR(state(r2, r1, r3, r1), 5, Action).

% Extra tests 
start2(Action) :- solveR(state(r1, r3, r1, r1), 3, Action).
start3(Action) :- solveR(state(r1, r3, r3, r1), 5, Action).