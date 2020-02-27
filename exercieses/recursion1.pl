on_route(rome).

on_route(Place):-
  move(Place, Method, NewPlace),
  on_route(NewPlace).

move(home,taxi,halifax).
move(halifax,train,gatwick).
move(gatwick,plane,rome).

