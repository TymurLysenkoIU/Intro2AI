:- dynamic solvegame/2.

solutionstofile(FilePath) :-
  open(FilePath, write, Stream),
  findall((A, N), solvegame(A, N), L),
  maplist([Sol]>>(write(Stream, Sol), nl(Stream)), L),
  close(Stream).

solutionsordered(L) :-
  setof((N, A), solvegame(A, N), L).

% ['backtrack.pl'].
% ['../test/prolog/1-from-task.pl'].
% ['../script/debug_tools.pl'].

% findall(N, solvegame(A, N), L), length(L, Len), min_list(L, M).
