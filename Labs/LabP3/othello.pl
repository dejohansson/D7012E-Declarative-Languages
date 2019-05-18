/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: David Johansson 
%    Student user id  : davjom-5
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').

%Random player 1 AI.
%:- ensure_loaded('stupid.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 


% opp(Player, Opponent)
opp(1, 2).
opp(2, 1).

len([], 0).
len([_|T], L) :- len(T, L2), L is L2+1.

% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ]).

initTestBoard([ [2,.,.,2,.,1], 
				[2,2,2,1,1,1],
				[2,2,2,2,2,1], 
				[2,1,2,2,2,1], 
				[1,1,2,1,1,1], 
				[2,2,2,2,2,1] ]).

testBoard1([ [.,.,.,.,.,.], 
			[.,1,.,.,.,.],
			[.,.,2,1,.,.],
			[.,.,1,2,.,.],
			[.,.,.,.,1,.],
			[.,.,.,.,.,.] ]).

testBoard2([ [.,2,.,.,.,2], 
			[.,.,1,.,1,.],
			[.,.,.,1,.,.],
			[.,.,1,1,1,.],
			[.,1,.,1,.,.],
			[.,.,.,2,.,.] ]).

testBoard3([ [.,.,.,2,.,.], 
			[.,2,.,1,1,.],
			[2,1,1,1,.,.],
			[2,1,1,.,1,2],
			[.,1,.,1,.,.],
			[2,.,.,2,2,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(B, 1) :- initBoard(B).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

winner(State, 1) :-
	terminal(State),
	points(1, State, P1),
	points(2, State, P2),
	P1 < P2.
winner(State, 2) :-
	terminal(State),
	points(1, State, P1),
	points(2, State, P2),
	P2 < P1.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :-
	terminal(State),
	points(1, State, Points),
	points(2, State, Points).

points(Plyr, State, Points) :-
	countP(Plyr, State, [5,5], 0, Points).

countP(_, _, [_,-1], Points, Points).
countP(Plyr, State, [X,Y], SoFar, Points) :-
	Y > -1,
	countLine(Plyr, State, [X,Y], SoFar, LnPoints),
	Y2 is Y-1,
	countP(Plyr, State, [X,Y2], LnPoints, Points).

countLine(_, _, [-1,_], LnPoints, LnPoints).
countLine(Plyr, State, [X,Y], SoFar, LnPoints) :-
	X > -1,
	point(Plyr, State, [X,Y], P), !,
	X2 is X-1,
	NewP is SoFar+P,
	countLine(Plyr, State, [X2,Y], NewP, LnPoints).
countLine(Plyr, State, [X,Y], SoFar, LnPoints) :-
	X > -1,
	X2 is X-1,
	countLine(Plyr, State, [X2,Y], SoFar, LnPoints).

point(Plyr, State, [X,Y], 1) :-
	get(State, [X, Y], Plyr).
point(Plyr, State, [X,Y], 0) :-
	not(get(State, [X, Y], Plyr)).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State) :-
	moves(1, State, []), !,
	moves(2, State, []).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

moves(Plyr, State, MvList) :-
	testMoves(Plyr, State, [5,5], [], MvList).

testMoves(_, _, [_,-1], [], ['n']).
testMoves(_, _, [_,-1], MvList, MvList).
testMoves(Plyr, State, [X,Y], SoFar, MvList) :-
	Y > -1,
	testLine(Plyr, State, [X,Y], SoFar, LnList),
	Y2 is Y-1,
	testMoves(Plyr, State, [X,Y2], LnList, MvList).

testLine(_, _, [-1,_], LnList, LnList).
testLine(Plyr, State, [X,Y], SoFar, LnList) :-
	X > -1,
	validmove(Plyr, State, [X,Y]), !,
	X2 is X-1,
	testLine(Plyr, State, [X2,Y], [[X,Y] | SoFar], LnList).
testLine(Plyr, State, [X,Y], SoFar, LnList) :-
	X > -1,
	X2 is X-1,
	testLine(Plyr, State, [X2,Y], SoFar, LnList).
	
% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

nextState(Plyr, 'n', State, State, NextPlyr) :- opp(Plyr, NextPlyr).
nextState(Plyr, Move, State, NewState, NextPlyr) :-
	opp(Plyr, NextPlyr),
	nextDir(Plyr, State, Move, [0, 1], S1),
	nextDir(Plyr, S1, Move, [0, -1], S2),
	nextDir(Plyr, S2, Move, [1, 0], S3),
	nextDir(Plyr, S3, Move, [1, 1], S4),
	nextDir(Plyr, S4, Move, [1, -1], S5),
	nextDir(Plyr, S5, Move, [-1, 0], S6),
	nextDir(Plyr, S6, Move, [-1, 1], S7),
	nextDir(Plyr, S7, Move, [-1, -1], NewState).

nextDir(Plyr, State, [X, Y], [Xi, Yi], State) :-
	not(updDir(Plyr, State, [X, Y], [Xi, Yi], _)).
nextDir(Plyr, State, [X, Y], [Xi, Yi], NewState) :-
	updDir(Plyr, State, [X, Y], [Xi, Yi], NewState).
%
updDir(Plyr, State, [X, Y], [Xi, Yi], NewState) :-
	X2 is X+Xi,
	Y2 is Y+Yi,
	get(State, [X2, Y2], Opp),
	opp(Plyr, Opp),
	updDirH(Plyr, State, [X2, Y2], [Xi, Yi], TempState),
	set(TempState, NewState, [X, Y], Plyr).
%
updDirH(Plyr, State, [X, Y], [Xi, Yi], NewState) :-
	updDir(Plyr, State, [X, Y], [Xi, Yi], NewState).
updDirH(Plyr, State, [X, Y], [Xi, Yi], NewState) :-
	X2 is X+Xi,
	Y2 is Y+Yi,
	get(State, [X2, Y2], Plyr),
	set(State, NewState, [X, Y], Plyr).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr, State, Proposed) :-
	get(State, Proposed, Value),
	Value = '.',
	check(Plyr, State, Proposed).
validmove(Plyr, State, 'n') :-
	moves(Plyr, State, ['n']).

check(Plyr, State, [X, Y]) :- 
	checkDir(Plyr, State, [X, Y], [0, -1]).
check(Plyr, State, [X, Y]) :- 
	checkDir(Plyr, State, [X, Y], [0, 1]).
check(Plyr, State, [X, Y]) :- 
	checkDir(Plyr, State, [X, Y], [1, 0]).
check(Plyr, State, [X, Y]) :- 
	checkDir(Plyr, State, [X, Y], [1, 1]).
check(Plyr, State, [X, Y]) :- 
	checkDir(Plyr, State, [X, Y], [1, -1]).
check(Plyr, State, [X, Y]) :- 
	checkDir(Plyr, State, [X, Y], [-1, 0]).
check(Plyr, State, [X, Y]) :- 
	checkDir(Plyr, State, [X, Y], [-1, 1]).
check(Plyr, State, [X, Y]) :- 
	checkDir(Plyr, State, [X, Y], [-1, -1]).

checkDir(Plyr, State, [X, Y], [Xi, Yi]) :-
	X2 is X+Xi,
	Y2 is Y+Yi,
	get(State, [X2, Y2], Opp),
	opp(Plyr, Opp),
	checkDirH(Plyr, State, [X2, Y2], [Xi, Yi]).

checkDirH(Plyr, State, [X, Y], [Xi, Yi]) :- 
	checkDir(Plyr, State, [X, Y], [Xi, Yi]).
checkDirH(Plyr, State, [X, Y], [Xi, Yi]) :- 
	X2 is X+Xi,
	Y2 is Y+Yi,
	get(State, [X2, Y2], Plyr).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State, P) :- 
	not(terminal(State)), !,
	%corner(State, P).
	stableCount(State, P1, P2), !,
	P is P2.
h(State, 0) :- tie(State), !.
h(State, 100) :- winner(State, 1), !.
h(State, -100) :- winner(State, 2), !.

stableCount(State, P1, P2) :-
	testStable(State, [5,5], 0, 0, P1, P2).

testStable(_, [_,-1], P1, P2, P1, P2).
testStable(State, [X,Y], P1t, P2t, P1, P2) :-
	Y > -1,
	testLineS(State, [X,Y], P1t, P2t, P1n, P2n),
	Y2 is Y-1,
	testStable(State, [X,Y2], P1n, P2n, P1, P2).

testLineS(_, [-1,_], P1n, P2n, P1n, P2n).
testLineS(State, [X,Y], P1t, P2t, P1n, P2n) :-
	X > -1,
	stable(1, State, [X, Y]), !,
	P1temp is P1t+1,
	X2 is X-1,
	testLineS(State, [X2,Y], P1temp, P2t, P1n, P2n).
testLineS(State, [X,Y], P1t, P2t, P1n, P2n) :-
	X > -1,
	stable(2, State, [X, Y]), !,
	P2temp is P2t+1,
	X2 is X-1,
	testLineS(State, [X2,Y], P1t, P2temp, P1n, P2n).
testLineS(State, [X,Y], P1t, P2t, P1n, P2n) :-
	X > -1,
	X2 is X-1,
	testLineS(State, [X2,Y], P1t, P2t, P1n, P2n).

stable(Plyr, State, [X, Y]) :-
	get(State, [X, Y], Plyr),
	stableAx(Plyr, State, [X, Y], [1, 0], [-1, 0]), !,
	stableAx(Plyr, State, [X, Y], [0, 1], [0, -1]), !,
	stableAx(Plyr, State, [X, Y], [-1, 1], [1, -1]), !,
	stableAx(Plyr, State, [X, Y], [-1, -1], [1, 1]), !.

stableAx(Plyr, State, [X, Y], [X1, Y1], [X2, Y2]) :-
	(filledDir(Plyr, State, [X, Y], [X1, Y1]), filledDir(Plyr, State, [X, Y], [X2, Y2])), !.
stableAx(_, State, [X, Y], [X1, Y1], _) :-
	X1n is X+X1, Y1n is Y+Y1,
	not(get(State, [X1n, Y1n], _)), !.
stableAx(_, State, [X, Y], _, [X2, Y2]) :-
	X2n is X+X2, Y2n is Y+Y2,
	not(get(State, [X2n, Y2n], _)), !.
/* stableAx(Plyr, State, [X, Y], [X1, Y1], _) :-
	X1n is X+X1, Y1n is Y+Y1,
	stable(Plyr, State, [X1n, Y1n]), !.
stableAx(Plyr, State, [X, Y], _, [X2, Y2]) :-
	X2n is X+X2, Y2n is Y+Y2,
	stable(Plyr, State, [X2n, Y2n]), !. */

filledDir(_, State, [X, Y], [Xi, Yi]) :- 
	X2 is X+Xi,
	Y2 is Y+Yi,
	not(get(State, [X2, Y2], _)), !.
filledDir(Plyr, State, [X, Y], [Xi, Yi]) :-
	X2 is X+Xi,
	Y2 is Y+Yi,
	(get(State, [X2, Y2], 1) ; get(State, [X2, Y2], 2)), !,
	filledDir(Plyr, State, [X2, Y2], [Xi, Yi]).

corner(State, P) :-
	posPoint(State, [0, 0], P1),
	posPoint(State, [0, 5], P2),
	posPoint(State, [5, 0], P3),
	posPoint(State, [5, 5], P4),
	P is P1+P2+P3+P4.

posPoint(State, [X, Y], 0) :- get(State, [X, Y], '.').
posPoint(State, [X, Y], 1) :- get(State, [X, Y], 2).
posPoint(State, [X, Y], -2) :- get(State, [X, Y], 1).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-101).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(101).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
