solutionstofile(FilePath) :-
  open(FilePath, write, Stream),
  findall((A, N), solvegame(A, N), L),
  maplist([Sol]>>(write(Stream, Sol), nl(Stream)), L),
  close(Stream).

solutionsordered(L) :-
  setof((N, A), solvegame(A, N), L).

% findall(N, solvegame(A, N), L), length(L, Len).
