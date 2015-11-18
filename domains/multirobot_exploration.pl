agent(r1).
agent(r2).

location(l1).
location(l2).
location(l3).
location(l4).
location(l5).

fluent(at(A,L)) :- agent(A), location(L).
fluent(visited(L)) :- location(L).

action(go(A,L2)) :- agent(A), location(L2).

causes(go(A,L2), at(A,L2), [at(A,L1)]) :- action(go(A,L2)), fluent(at(A,L1)), fluent(at(A,L2)), diff(L1,L2).
%causes(go(A,L2), visited(L2), []) :- action(go(A,L2)), fluent(visited(L2)).

caused([at(A,L)], mneg(at(A,L1))) :- fluent(at(A,L)), location(L1), diff(L,L1).
caused([at(A,L)], visited(L)) :- fluent(at(A,L)), fluent(visited(L)).

executable(go(A,L2), []) :- action(go(A,L2)).

initially(at(A,l1)) :- agent(A).
initially(mneg(visited(L))) :- location(L), diff(L,l1).

goal(visited(L)) :- location(L).

