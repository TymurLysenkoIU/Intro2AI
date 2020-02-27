taller(bob, mike).
taller(mike, jim).
taller(jim, george).

tallest(X):-
  taller(bob, X).

tallest(X):-
  taller(Y, X),
  tallest(Y).

