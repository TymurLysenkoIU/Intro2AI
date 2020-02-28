% A backtrack solution to the task

% Library for lambdas
use_module(library(yall)).
% Library with dictionaries utilities
% use_module(library(dicts)).

t(Xt, Yt).
h(Xh, Yh).
o(Xo, Yo).

minX(0).
maxX(20).
minY(0).
maxY(20).

% Checks if the move is legal
ismoveavailable(
  (X, Y), % coordinates to validate
  Visited % list of visited cells by a player
) :-
  minX(MinX),
  maxX(MaxX),
  minY(MinY),
  maxY(MaxY),
  X >= MinX,
  X =< MaxX,
  Y >= MinY,
  Y =< MaxY,
  not(member((X, Y), Visited)).

% Having current coordinates prove coordinates where it is possible to move
availablemoves(
  (CurX, CurY), % current x and y coodinates of the player holding the ball
  Visited, % list of visited cells by a player
  (NewX, CurY) % new coordinates to move to according to rules
) :-
  % Increment x coordinate
  NewX is CurX + 1,
  ismoveavailable((NewX, CurY), Visited).

availablemoves((CurX, CurY), Visited, (NewX, CurY)) :-
  % Decrement X coordinate
  NewX is CurX - 1,
  ismoveavailable((NewX, CurY), Visited).

availablemoves((CurX, CurY), Visited, (CurX, NewY)) :-
  % Increment Y coordinate
  NewY is CurY + 1,
  ismoveavailable((CurX, NewY), Visited).

availablemoves((CurX, CurY), Visited, (CurX, NewY)) :-
  % Decrement Y coordinate
  NewY is CurY - 1,
  ismoveavailable((CurX, NewY), Visited).

% Finds the solution to the game
solvegamerec(
  (CurX, CurY),
  PassHappened,
  Visited,
  Actions,
  NumMoves
) :-
  % Base case: CurX, CurY correspond to the coordinates of the touchdown point - the game is won
  t(CurX, CurY).

solvegamerec(
  (CurX, CurY), % current x and y coodinates of the player holding the ball
  PassHappened, % value which is true, if a pass has hapenned during the play
  Visited, % list of visited cells by a player
  Actions, % list of pairs (action_type, (x, y))
  NumMoves % number of moves done so far
) :-
  true.

% Function that solves the game and returns the path along with its length
solvegame(Visited, NumMoves) :-
  solvegamerec((0, 0), (0, 0), false, Visited, NumMoves).
