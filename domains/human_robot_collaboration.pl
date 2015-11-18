%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Robot collaboration domain:
%%%
%%% initial situation:
%%%
%%% R1,R2
%%%    L1 * circle                  L2 * triangle
%%%
%%%    O1             O2
%%%    L5 * square    L4 * square   L3 * triangle
%%%
%%%
%%%
%%% Robots:    R1,R2
%%% Objects:   O1,O2
%%% Locations: L1,L2,L3,L4,L5
%%%
%%% R1 can go in circle, square.
%%% R2 can go in circle, triangle
%%%
%%% Goal:
%%% bring O1 in L2, and O2 in L3 (optimal plan: 13 actions)

agent(r1).
agent(r2).

agent_capability(r1,circle).
agent_capability(r1,square).
agent_capability(r2,circle).
agent_capability(r2,triangle).

location(l1).
location(l2).
location(l3).
location(l4).
location(l5).

location_class(l1,circle).
location_class(l2,triangle).
location_class(l3,triangle).
location_class(l4,square).
location_class(l5,square).

object(o1).
object(o2).

fluent(at(A,L)) :- agent(A), location(L).
fluent(visited(L)) :- location(L).
fluent(object_at(O,L)) :- object(O), location(L).
fluent(holding(A,O)) :- agent(A), object(O).

action(go(A,L)) :- agent(A), location(L), location_class(L,C), agent_capability(A,C).
action(pick(A,O)) :- agent(A), object(O).
action(drop(A,O)) :- agent(A), object(O).
action(handover(A,O,A1)) :- agent(A), object(O), agent(A1), diff(A,A1).

causes(go(A,L), at(A,L), []) :- action(go(A,L)), fluent(at(A,L)).
causes(pick(A,O), holding(A,O), []) :- action(pick(A,O)), fluent(holding(A,O)).
causes(pick(A,O), mneg(object_at(O,L)), [object_at(O,L)]) :- action(pick(A,O)), fluent(object_at(O,L)).
causes(drop(A,O), object_at(O,L), [at(A,L)]) :- action(drop(A,O)), fluent(object_at(O,L)), fluent(at(A,L)).
causes(drop(A,O), mneg(holding(A,O)), []) :- action(drop(A,O)), fluent(holding(A,O)).
causes(handover(A,O,A1), mneg(holding(A,O)), []) :- action(handover(A,O,A1)), fluent(holding(A,O)).
causes(handover(A,O,A1), holding(A1,O), []) :- action(handover(A,O,A1)), fluent(holding(A1,O)).

caused([at(A,L)], mneg(at(A,L1))) :- fluent(at(A,L)), fluent(at(A,L1)), diff(L,L1).
caused([at(A,L)], visited(L)) :- fluent(at(A,L)), fluent(visited(L)).
caused([object_at(O,L)], mneg(object_at(O,L1))) :- fluent(object_at(O,L)), fluent(object_at(O,L1)), diff(L,L1).
caused([object_at(O,L)], mneg(holding(A,O))) :- fluent(object_at(O,L)), fluent(holding(A,O)).
caused([holding(A,O)], mneg(object_at(O,L))) :- fluent(holding(A,O)), fluent(object_at(O,L)).
caused([holding(A,O)], mneg(holding(A1,O))) :- fluent(holding(A,O)), fluent(holding(A1,O)), diff(A,A1).

executable(go(A,L), [mneg(at(A,L))]) :- action(go(A,L)), fluent(at(A,L)).
executable(pick(A,O), [at(A,L), object_at(O,L), mneg(holding(A,O1))]) :- action(pick(A,O)), fluent(at(A,L)), fluent(object_at(O,L)), fluent(holding(A,O1)), diff(O,O1).
executable(drop(A,O), [holding(A,O)]) :- action(drop(A,O)), fluent(holding(A,O)).
executable(handover(A,O,A1), [at(A,L), at(A1,L), holding(A,O), mneg(holding(A1,O1))]) :- action(handover(A,O,A1)), fluent(at(A,L)), fluent(at(A1,L)), fluent(holding(A,O)), fluent(holding(A1,O1)), diff(O,O1).

initially(at(A,l1)) :- agent(A).
initially(object_at(o1,l5)).
initially(object_at(o2,l4)).
initially(mneg(visited(L))) :- location(L), diff(L,l1).

%goal(visited(L)) :- location(L).
goal(object_at(o1,l2)).
goal(object_at(o2,l3)).

