%%% "wolf goat cabbage" puzzle
% optimal solution takes 20 actions

object(wolf).
object(goat).
object(cabbage).
object(man).

side(left).
side(right).

location(X) :- side(X).
location(boat).

fluent(at(X,Y)) :- object(X), location(Y).
fluent(boat_at(Y)) :- side(Y).
fluent(alive).

action(navigate(A,B)) :-  side(A), side(B), diff(A,B).
action(pick(A)) :- object(A).
action(drop(A)) :- object(A).

executable(navigate(A,B), [boat_at(A),at(man,boat)]) :- action(navigate(A,B)). 
executable(pick(A), [boat_at(L),at(A,L)]) :- action(pick(A)).
executable(drop(A), [at(A,boat)]) :- action(drop(A)).

causes(navigate(A,B), boat_at(B), []) :- action(navigate(A,B)), fluent(boat_at(B)).
causes(pick(A), at(A,boat), []) :- action(pick(A)), fluent(at(A,boat)).
causes(drop(A), at(A,L), [boat_at(L)]) :- action(drop(A)), fluent(at(A,L)), fluent(boat_at(L)).

initially(at(A,left)) :- object(A).
initially(boat_at(left)).
initially(alive).

goal(at(A,right)) :- object(A).
goal(alive).

caused([at(O,L1)], mneg(at(O,L2))) :- fluent(at(O,L1)), fluent(at(O,L2)), diff(L1,L2).
caused([boat_at(L1)], mneg(boat_at(L2))) :- fluent(boat_at(L1)), fluent(boat_at(L2)), diff(L1,L2).

% no more than 1 objects other than man on boat
caused([at(A,boat),at(B,boat)], mneg(alive)) :- fluent(at(A,boat)), fluent(at(B,boat)), diff(A,B,man).

caused([at(wolf,L),at(goat,L),mneg(at(man,L))], mneg(alive)) :- location(L).
caused([at(cabbage,L),at(goat,L),mneg(at(man,L))],mneg(alive)) :- location(L).

