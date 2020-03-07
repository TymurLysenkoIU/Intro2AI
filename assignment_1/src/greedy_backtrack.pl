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

% Checks if the coordinates are within the boundaries of the game field
arecoordinateswithinboundaries(
  (X, Y) % coordinates to validate
) :-
  minX(MinX),
  maxX(MaxX),
  minY(MinY),
  maxY(MaxY),
  X >= MinX,
  X =< MaxX,
  Y >= MinY,
  Y =< MaxY.

% Checks if the move is legal
ismoveavailable(
  (X, Y), % coordinates to validate
  Visited % list of visited cells by a player
) :-
  % The move is legal, if
  % 1) it is within the boundaries
  arecoordinateswithinboundaries((X, Y)),
  % 2) the coordinates have not already been visited
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

% Check if there is a touchdown point near the current one
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

% Checks if coordinates of the ball are valid, i. e. not ork and not out of the boundaries
isvalidpositionofball(
  (X, Y), % coordinates of the ball
  Visited % list of visited points
) :-
  not(o(X, Y)),
  arecoordinateswithinboundaries((X, Y)),
  % no need for the ball to fly beyond the visited points,
  % since the more efficient solution will be checked, when
  % a ball will be passed from these coordinates
  not(member((X, Y), Visited)).

% Checks if the ball can be thrown in the given direction (specified by the velocity vector)
ispassavailableindirection(
  (CurX, CurY),
  Visited,
  (1, 0) % Right velocity vector
) :-
  isvalidpositionofball((CurX + 1, CurY), Visited).

ispassavailableindirection(
  (CurX, CurY),
  Visited,
  (0, 1) % Top velocity vector
) :-
  isvalidpositionofball((CurX, CurY + 1), Visited).

ispassavailableindirection(
  (CurX, CurY),
  Visited,
  (1, 1) % Right top velocity vector
) :-
  isvalidpositionofball((CurX + 1, CurY + 1), Visited).

ispassavailableindirection(
  (CurX, CurY),
  Visited,
  (-1, 0) % Left velocity vector
) :-
  isvalidpositionofball((CurX - 1, CurY), Visited).

ispassavailableindirection(
  (CurX, CurY),
  Visited,
  (0, -1) % Bottom velocity vector
) :-
  isvalidpositionofball((CurX, CurY - 1), Visited).

ispassavailableindirection(
  (CurX, CurY),
  Visited,
  (-1, -1) % Bottom left velocity vector
) :-
  isvalidpositionofball((CurX - 1, CurY - 1), Visited).

ispassavailableindirection(
  (CurX, CurY),
  Visited,
  (1, -1) % Bottom right velocity vector
) :-
  isvalidpositionofball((CurX + 1, CurY - 1), Visited).

ispassavailableindirection(
  (CurX, CurY),
  Visited,
  (-1, 1) % Top left velocity vector
) :-
  isvalidpositionofball((CurX - 1, CurY + 1), Visited).

% Trace the ball in the given direction (specified by the velocity vecror) and assign the coordinates
% of the first encountered human, if it was not intercepted or flew out of bounds
traceballpass(
  (CurX, CurY), % current position of the ball
  (VelocityX, VelocityY), % velocity vector of the ball
  _,
  (ResultX, ResultY) % coordinates of the human who caught the ball, in case of success
) :-
  ResultX is CurX + VelocityX,
  ResultY is CurY + VelocityY,
  h(ResultX, ResultY),
  !.

traceballpass(
  (CurX, CurY), % current position of the ball
  (VelocityX, VelocityY), % velocity vector of the ball
  Visited,
  (ResultX, ResultY) % coordinates of the human who caught the ball, in case of success
) :-
  % Recursive case: add velocity of the ball to the coordinates and check the new posiotion of the ball
  NewX is CurX + VelocityX,
  NewY is CurY + VelocityY,
  isvalidpositionofball((NewX, NewY), Visited),
  traceballpass((NewX, NewY), (VelocityX, VelocityY), Visited, (ResultX, ResultY)).

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
  OutActions, % output actions of the solution
  OutNumMoves % output number of actions of the solution
) :-
  % Recursive case - move to an adjacent point
  nb_getval(min_moves_so_far, MinMovesSoFar),
  MinMovesSoFar >= NumMoves,
  possiblemoves((CurX, CurY), Visited, (NewX, NewY)), % get new coordinates to move in
  not(o(NewX, NewY)), % if orc stands on the new coordinates - discard the solution
  ( h(NewX, NewY) ->
    (
      NewActions = Actions,
      NewNumMoves = NumMoves
    );
    (
      NewActions = [(move, (NewX, NewY)) | Actions], % add move to new coordinates to the lsit of actions
      NewNumMoves is NumMoves + 1 % increment the number of actions
    )
  ),
  solvegamerec(
    (NewX, NewY),
    PassHappened,
    [(CurX, CurY) | Visited],
    NewActions,
    NewNumMoves,
    OutActions,
    OutNumMoves
  ).

solvegamerec(
  (CurX, CurY), % current x and y coodinates of the player holding the ball
  false, % value which is true, if a pass has hapenned during the play
  Visited, % list of visited cells by a player
  Actions, % list of pairs (action_type, (x, y))
  NumMoves, % number of moves done so far
  OutActions, % output actions of the solution
  OutNumMoves % output number of actions of the solution
) :-
  % Recursive case - pass the ball
  nb_getval(min_moves_so_far, MinMovesSoFar),
  MinMovesSoFar >= NumMoves,
  ispassavailableindirection((CurX, CurY), Visited, VelocityVec), % get the direction to throw the ball in
  traceballpass((CurX, CurY), VelocityVec, Visited, (NewX, NewY)), % get the new coordinates of a human who caught the ball
  NewActions = [(pass, (NewX, NewY)) | Actions], % add move to new coordinates to the lsit of actions
  NewNumMoves is NumMoves + 1, % increment the number of actions
  solvegamerec((NewX, NewY), true, Visited, NewActions, NewNumMoves, OutActions, OutNumMoves).

% Function that solves the game and returns the path along with its length
solvegame(Actions, NumMoves) :-
  maxX(MaxX),
  maxY(MaxY),
  MinMovesSoFar is MaxX * MaxY,
  nb_setval(min_moves_so_far, MinMovesSoFar),
  solvegamerec((0, 0), false, [], [], 0, Actions, NumMoves).

solvegamewithstatistics(Solutions, RunTime) :-
  statistics(runtime, [StartTime, _]),
  setof((NumActs, Actions), solvegame(Actions, NumActs), Solutions),
  statistics(runtime, [EndTime, _]),
  RunTime is EndTime - StartTime.

% Run the function to calculate the best solution of a map and print it
main() :-
  solvegamewithstatistics(Result, RunTime) ->
  (
    nth0(0, Result, (SolLen, RevSol)), % The best solution is the first in Result, since it is sorted by the length
    reverse(RevSol, Sol),
    format('~d~n', [SolLen]),
    maplist([(ActionType, X, Y)] >> (
        ActionType = pass ->
          format('P ~d ~d~n', [X, Y])
        ;
          format('~d ~d~n', [X, Y])
      ),
      Sol
    ),
    format('~d msec~n', [RunTime])
  )
  ;
  (
    format('Not able to solve the map.')
  ).
