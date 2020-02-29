% A backtrack solution to the task

% Library for lambdas
use_module(library(yall)).
% Library with dictionary utilities
% use_module(library(dicts)).

h(_, _) :- false.
o(_, _) :- false.
t(_, _) :- false.

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

% check if there is a touchdown point near the current one
hastouchdownnearby(
  (CurX, CurY), % current x and y coodinates of the player holding the ball
  (NewX, CurY) % coordinates of the touchdown point
) :-
  % Increment X coordinate
  NewX is CurX + 1,
  t(NewX, CurY).

hastouchdownnearby((CurX, CurY), (NewX, CurY)) :-
  % Decrement X coordinate
  NewX is CurX - 1,
  t(NewX, CurY).

hastouchdownnearby((CurX, CurY), (CurX, NewY)) :-
  % Increment Y coordinate
  NewY is CurY + 1,
  t(CurX, NewY).

hastouchdownnearby((CurX, CurY), (CurX, NewY)) :-
  % Decrement Y coordinate
  NewY is CurY - 1,
  t(CurX, NewY).

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
  nb_getval(min_moves_so_far, MinMovesSoFar),
  min_list([MinMovesSoFar, NumMoves], Min),
  nb_setval(min_moves_so_far, Min),
  !.

solvegamerec(
  (CurX, CurY),
  PassHappened,
  Visited,
  Actions,
  NumMoves,
  OutActions,
  OutNumMoves
) :-
  % Base case: the touchdown point is near
  hastouchdownnearby((CurX, CurY), (NewX, NewY)),
  OutActions = [(move, (NewX, NewY)) | Actions],
  OutNumMoves is NumMoves + 1,
  nb_getval(min_moves_so_far, MinMovesSoFar),
  min_list([MinMovesSoFar, NumMoves], Min),
  nb_setval(min_moves_so_far, Min),
  !.

solvegamerec(
  (CurX, CurY), % current x and y coodinates of the player holding the ball
  PassHappened, % value which is true, if a pass has hapenned during the play
  Visited, % list of visited cells by a player
  Actions, % list of pairs (action_type, (x, y))
  NumMoves, % number of moves done so far
  OutActions,
  OutNumMoves
) :-
  % Recursive case - move to an adjacent point
  nb_getval(min_moves_so_far, MinMovesSoFar),
  MinMovesSoFar >= NumMoves,
  possiblemoves((CurX, CurY), Visited, (NewX, NewY)),
  not(o(NewX, NewY)), % if orc stands on the new coordinates - discard the solution
  NewActions = [(move, (NewX, NewY)) | Actions],
  NewNumMoves is NumMoves + 1,
  solvegamerec((NewX, NewY), false, [(CurX, CurY) | Visited], NewActions, NewNumMoves, OutActions, OutNumMoves).

% Function that solves the game and returns the path along with its length
solvegame(Actions, NumMoves) :-
  maxX(MaxX),
  maxY(MaxY),
  MinMovesSoFar is MaxX * MaxY,
  nb_setval(min_moves_so_far, MinMovesSoFar),
  solvegamerec((0, 0), false, [], [], 0, Actions, NumMoves).
