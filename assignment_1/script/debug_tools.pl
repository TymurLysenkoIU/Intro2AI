:- dynamic solvegame/2.

solutionstofile(FilePath) :-
  open(FilePath, write, Stream),
  findall((A, N), solvegame(A, N), L),
  maplist([Sol]>>(write(Stream, Sol), nl(Stream)), L),
  close(Stream).

solutionsordered(L) :-
  setof((N, A), solvegame(A, N), L).

thebestsolution(Len, Sol) :-
  solutionsordered(L), nth0(0, L, (Len, Sol)).

% set_prolog_flag(answer_write_options,[max_depth(0)]).

% ['greedy_backtrack.pl'].
% ['../test/prolog/1-from-task.pl'].
% ['../script/debug_tools.pl'].

% findall(N, solvegame(A, N), L), length(L, Len), min_list(L, M).
