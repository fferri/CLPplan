%%% "wolf goat cabbage" puzzle
% optimal solution takes # actions

object(wolf).
object(goat).
object(cabbage).

side(left).
side(right).

fluent(at(X,Y)) :- object(X), side(Y).
fluent(boat_at(Y)) :- side(Y).
fluent(alive).

action(navigate(A,B)) :-  side(A), side(B), diff(A,B).
action(navigate(A,B,O)) :-  side(A), side(B), diff(A,B), object(O).

executable(navigate(A,B), [boat_at(A)]) :- action(navigate(A,B)). 
executable(navigate(A,B,O), [boat_at(A),at(O,A)]) :- action(navigate(A,B,O)). 

causes(navigate(A,B), boat_at(B), []) :- action(navigate(A,B)), fluent(boat_at(B)).
causes(navigate(A,B,O), boat_at(B), []) :- action(navigate(A,B)), fluent(boat_at(B)).
causes(navigate(A,B,O), at(O,B), []) :- action(navigate(A,B)), fluent(at(O,B)).

initially(at(A,left)) :- object(A).
initially(boat_at(left)).
initially(alive).

%goal(at(A,right)) :- object(A).
goal(at(wolf,right)).
%goal(at(cabbage,right)).
goal(alive).

caused([at(O,L1)], mneg(at(O,L2))) :- fluent(at(O,L1)), fluent(at(O,L2)), diff(L1,L2).
caused([boat_at(L1)], mneg(boat_at(L2))) :- fluent(boat_at(L1)), fluent(boat_at(L2)), diff(L1,L2).

caused([at(wolf,L),at(goat,L),mneg(boat_at(L))], mneg(alive)) :- side(L).
caused([at(cabbage,L),at(goat,L),mneg(boat_at(L))], mneg(alive)) :- side(L).

