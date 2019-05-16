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

% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ]).

initTestBoard([ [.,.,.,.,.,.], 
				[.,.,.,.,.,.],
				[.,.,1,.,.,.], 
				[.,.,.,.,.,.], 
				[.,.,.,.,.,.], 
				[.,.,.,.,.,.] ]).

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
	nextNorth(Plyr, State, Move, S1),
	nextSouth(Plyr, S1, Move, S2),
	nextWest(Plyr, S2, Move, S3),
	nextEast(Plyr, S3, Move, NewState).

nextNorth(Plyr, State, [X, Y], State) :-
	not(updNorth(Plyr, State, [X, Y], _)).
nextNorth(Plyr, State, [X, Y], NewState) :-
	updNorth(Plyr, State, [X, Y], NewState).
%
updNorth(Plyr, State, [X, Y], NewState) :-
	Y2 is Y-1,
	get(State, [X, Y2], Opp),
	opp(Plyr, Opp),
	updNorthHelp(Plyr, State, [X, Y2], TempState),
	set(TempState, NewState, [X, Y], Plyr).
%
updNorthHelp(Plyr, State, [X, Y], NewState) :-
	Y2 is Y-1,
	get(State, [X, Y2], Opp),
	opp(Plyr, Opp),
	updNorthHelp(Plyr, State, [X, Y2], TempState),
	set(TempState, NewState, [X, Y], Plyr).
updNorthHelp(Plyr, State, [X, Y], NewState) :-
	Y2 is Y-1,
	get(State, [X, Y2], Plyr),
	set(State, NewState, [X, Y], Plyr).
%
nextSouth(Plyr, State, [X, Y], State) :-
	not(updSouth(Plyr, State, [X, Y], _)).
nextSouth(Plyr, State, [X, Y], NewState) :-
	updSouth(Plyr, State, [X, Y], NewState).
%
updSouth(Plyr, State, [X, Y], NewState) :-
	Y2 is Y+1,
	get(State, [X, Y2], Opp),
	opp(Plyr, Opp),
	updSouthHelp(Plyr, State, [X, Y2], TempState),
	set(TempState, NewState, [X, Y], Plyr).
%
updSouthHelp(Plyr, State, [X, Y], NewState) :-
	Y2 is Y+1,
	get(State, [X, Y2], Opp),
	opp(Plyr, Opp),
	updSouthHelp(Plyr, State, [X, Y2], TempState),
	set(TempState, NewState, [X, Y], Plyr).
updSouthHelp(Plyr, State, [X, Y], NewState) :-
	Y2 is Y+1,
	get(State, [X, Y2], Plyr),
	set(State, NewState, [X, Y], Plyr).
%
nextWest(Plyr, State, [X, Y], State) :-
	not(updWest(Plyr, State, [X, Y], _)).
nextWest(Plyr, State, [X, Y], NewState) :-
	updWest(Plyr, State, [X, Y], NewState).
%
updWest(Plyr, State, [X, Y], NewState) :-
	X2 is X-1,
	get(State, [X2, Y], Opp),
	opp(Plyr, Opp),
	updWestHelp(Plyr, State, [X2, Y], TempState),
	set(TempState, NewState, [X, Y], Plyr).
%
updWestHelp(Plyr, State, [X, Y], NewState) :-
	X2 is X-1,
	get(State, [X2, Y], Opp),
	opp(Plyr, Opp),
	updWestHelp(Plyr, State, [X2, Y], TempState),
	set(TempState, NewState, [X, Y], Plyr).
updWestHelp(Plyr, State, [X, Y], NewState) :-
	X2 is X-1,
	get(State, [X2, Y], Plyr),
	set(State, NewState, [X, Y], Plyr).
%
nextEast(Plyr, State, [X, Y], State) :-
	not(updEast(Plyr, State, [X, Y], _)).
nextEast(Plyr, State, [X, Y], NewState) :-
	updEast(Plyr, State, [X, Y], NewState).
%
updEast(Plyr, State, [X, Y], NewState) :-
	X2 is X+1,
	get(State, [X2, Y], Opp),
	opp(Plyr, Opp),
	updEastHelp(Plyr, State, [X2, Y], TempState),
	set(TempState, NewState, [X, Y], Plyr).
%
updEastHelp(Plyr, State, [X, Y], NewState) :-
	X2 is X+1,
	get(State, [X2, Y], Opp),
	opp(Plyr, Opp),
	updEastHelp(Plyr, State, [X2, Y], TempState),
	set(TempState, NewState, [X, Y], Plyr).
updEastHelp(Plyr, State, [X, Y], NewState) :-
	X2 is X+1,
	get(State, [X2, Y], Plyr),
	set(State, NewState, [X, Y], Plyr).
%
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
	moves(Plyr, State, []).

check(Plyr, State, [X, Y]) :- 
	Y2 is Y-1,
	get(State, [X, Y2], Opp),
	opp(Plyr, Opp),
	checkNorth(Plyr, State, [X, Y2]).
check(Plyr, State, [X, Y]) :- 
	Y2 is Y+1,
	get(State, [X, Y2], Opp),
	opp(Plyr, Opp),
	checkSouth(Plyr, State, [X, Y2]).
check(Plyr, State, [X, Y]) :- 
	X2 is X-1,
	get(State, [X2, Y], Opp),
	opp(Plyr, Opp),
	checkWest(Plyr, State, [X2, Y]).
check(Plyr, State, [X, Y]) :- 
	X2 is X+1,
	get(State, [X2, Y], Opp),
	opp(Plyr, Opp),
	checkEast(Plyr, State, [X2, Y]).
%
checkNorth(Plyr, State, [X, Y]) :- 
	Y2 is Y-1,
	get(State, [X, Y2], Opp),
	opp(Plyr, Opp),
	checkNorth(Plyr, State, [X, Y2]).
checkNorth(Plyr, State, [X, Y]) :- 
	Y2 is Y-1,
	get(State, [X, Y2], Plyr).
%
checkSouth(Plyr, State, [X, Y]) :- 
	Y2 is Y+1,
	get(State, [X, Y2], Opp),
	opp(Plyr, Opp),
	checkSouth(Plyr, State, [X, Y2]).
checkSouth(Plyr, State, [X, Y]) :- 
	Y2 is Y+1,
	get(State, [X, Y2], Plyr).
%
checkWest(Plyr, State, [X, Y]) :- 
	X2 is X-1,
	get(State, [X2, Y], Opp),
	opp(Plyr, Opp),
	checkWest(Plyr, State, [X2, Y]).
checkWest(Plyr, State, [X, Y]) :- 
	X2 is X-1,
	get(State, [X2, Y], Plyr).
%
checkEast(Plyr, State, [X, Y]) :- 
	X2 is X+1,
	get(State, [X2, Y], Opp),
	opp(Plyr, Opp),
	checkEast(Plyr, State, [X2, Y]).
checkEast(Plyr, State, [X, Y]) :- 
	X2 is X+1,
	get(State, [X2, Y], Plyr).
%

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

h(State, 0) :- not(terminal(State)).
h(State, 0) :- tie(State).
h(State, 100) :- winner(State, 1).
h(State, -100) :- winner(State, 2).

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
 
