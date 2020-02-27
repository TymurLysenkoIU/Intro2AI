parent(john,paul). /* paul is john's parent */
parent(paul,tom). /* tom is paul's parent */
parent(tom,mary). /* mary is tom's parent */

ancestor(X,Y):-
  parent(X,Y). /* someone is your ancestor if there are your parent */

ancestor(X,Y):-
  parent(X,Z), /* or somebody is your ancestor if they are the parent */
  ancestor(Z,Y). /* of someone who is your ancestor */

