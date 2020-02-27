road(town1, town2).
road(town2, town3).
road(town3, town4).
road(town4, town5).
road(town5, town6).

can_get(S, D):-
  road(S, D).

can_get(S, D):-
  road(S, T),
  can_get(T, D).

