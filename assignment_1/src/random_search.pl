% A backtrack solution to the task

% Library for lambdas
use_module(library(yall)).

:- dynamic h/2.
:- dynamic o/2.
:- dynamic t/2.

minX(0).
maxX(9).
minY(0).
maxY(9).

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
possiblemoves(
  (CurX, CurY), % current x and y coodinates of the player holding the ball
  Visited, % list of visited cells by a player
  (NewX, CurY) % new coordinates to move to according to rules
) :-
  % Increment x coordinate
  NewX is CurX + 1,
  ismoveavailable((NewX, CurY), Visited).

possiblemoves((CurX, CurY), Visited, (NewX, CurY)) :-
  % Decrement X coordinate
  NewX is CurX - 1,
  ismoveavailable((NewX, CurY), Visited).

possiblemoves((CurX, CurY), Visited, (CurX, NewY)) :-
  % Increment Y coordinate
  NewY is CurY + 1,
  ismoveavailable((CurX, NewY), Visited).

possiblemoves((CurX, CurY), Visited, (CurX, NewY)) :-
  % Decrement Y coordinate
  NewY is CurY - 1,
  ismoveavailable((CurX, NewY), Visited).

% Finds the solutions to the game
solvegamerec(
  (CurX, CurY),
  PassHappened,
  Visited,
  Actions,
  NumMoves,
  Actions,
  NumMoves
) :-
  % Base case: CurX, CurY correspond to the coordinates of the touchdown point - the game is won
  t(CurX, CurY),
  !.

% TODO: randomize
solvegamerec(
  (CurX, CurY), % current x and y coodinates of the player holding the ball
  PassHappened, % value which is true, if a pass has hapenned during the play
  Visited, % list of visited cells by a player
  Actions, % list of pairs (action_type, (x, y))
  NumMoves, % number of moves done so far
  OutActions,
  OutNumMoves
) :-
  % Recursive case - move
  % format('CurX = ~d CurY = ~d|~n', [CurX, CurY]),
  possiblemoves((CurX, CurY), Visited, (NewX, NewY)), % store possible moves in NewPoint
  % format('NewX = ~d NewY = ~d|~n', [NewX, NewY]),
  not(o(NewX, NewY)), % if orc stands on the new coordinates - discard the solution
  NewActions = [(move, (NewX, NewY)) | Actions],
  NewNumMoves is NumMoves + 1,
  solvegamerec((NewX, NewY), false, [(CurX, CurY) | Visited], NewActions, NewNumMoves, OutActions, OutNumMoves).

% TODO: pass the ball
% solvegamerec(
%   (CurX, CurY),
%   PassHappened,
%   Visited,
%   Actions,
%   NumMoves,
%   OutActions,
%   OutNumMoves
% ) :-

% Function that solves the game and returns the path along with its length
solvegame(Actions, NumMoves) :-
  solvegamerec((0, 0), false, [], [], 0, Actions, NumMoves).

% TODO: get first 100 attempts and find the minimal solution
