% class taxonomy

object(X) :- tool(X).
object(X) :- equipment(X).
%object(X) :- trolley(X).
object(X) :- toolbox(X).

%equipment(X) :- belt(X).
equipment(X) :- diverter(X).
equipment(X) :- diverter_guard(X).
equipment(X) :- roller_and_pulley(X).
%equipment(X) :- motor_and_shaft(X).
%equipment(X) :- drive_pulley(X).
%equipment(X) :- small_roller(X).

tool(X) :- allen_key(X).
tool(X) :- brush(X).
tool(X) :- bottle(X).
tool(X) :- cleaning_cloth(X).
tool(X) :- hammer(X).
tool(X) :- pliers(X).
%tool(X) :- pneumatic_cylinder(X).
%tool(X) :- pressure_roller(X).
tool(X) :- spanner(X).
tool(X) :- screwdriver(X).
tool(X) :- socket(X).
tool(X) :- knife(X).
tool(X) :- torch(X).

bottle(X) :- cleaning_fluid(X).

%brush(X) :- antistatic_brush(X).

hammer(X) :- mallet(X).

spanner(X) :- ratchet_spanner(X).

diverter_guard(X) :- diverter_guard_type1(X).
diverter_guard(X) :- diverter_guard_type2(X).

agent(X) :- human(X).

agent(X) :- robot(X).

% class properties

weight(X, 12.8) :- diverter_guard_type1(X).
weight(X, 6.4) :- diverter_guard_type2(X).
weight(X, 2.1) :- roller_and_pulley(X).
weight(X, 10.5) :- motor_and_shaft(X).
weight(X, 0.138) :- small_roller(X).
weight(X, 1.4) :- drive_pulley(X).

size(X, [0.86, 1.23]) :- diverter_guard_type1(X).
size(X, [0.82, 0.60]) :- diverter_guard_type2(X).

can_lift(X, Y) :- human(X), Y < 10.
can_lift(X, Y) :- robot(X), Y < 5.

min_radius(X, 0.4) :- human(X).
min_radius(X, 0.60) :- robot(X).

slope_tolerance(X, Y) :- robot(X), Y = 10*3.14159/180.

% instances

diverter(ck03).
diverter(ck11).
diverter(ck12).
diverter(ck20).

has_part(ck03, ck03_guard).
has_part(ck03, r1).
has_part(ck03, r2).
has_part(ck11, ck11_guard).
has_part(ck12, ck12_guard).
has_part(ck20, ck20_guard).

slope_requirement(ck11, Y) :- Y is 10*pi/180.
slope_requirement(ck12, Y) :- Y is 10*pi/180.

footprint_requirement(ck20, 0.65).

diverter_guard_type1(ck03_guard).
diverter_guard_type1(ck12_guard).
diverter_guard_type1(ck20_guard).

diverter_guard_type2(ck11_guard).

roller_and_pulley(r1).
roller_and_pulley(r2).

human(operator_1).

robot(robot_1).

toolbox(toolbox_1).
allen_key(allen_key_1).
cleaning_fluid(cleaning_fluid_bottle_1).
brush(generic_brush_1).
cleaning_cloth(cleaning_cloth_1).
mallet(mallet_1).
pliers(pliers_1).
ratchet_spanner(ratchet_spanner_set_1).
screwdriver(screwdriver_250mm_1).
screwdriver(screwdriver_200mm_1).
screwdriver(screwdriver_175mm_1).
socket(socket_1).
spanner(spanner_22mm_1).

contains(toolbox1,allen_key_1).
contains(toolbox1,cleaning_fluid_bottle_1).
contains(toolbox1,generic_brush_1).
contains(toolbox1,cleaning_cloth_1).
contains(toolbox1,mallet_1).
contains(toolbox1,pliers_1).
contains(toolbox1,ratchet_spanner_set_1).
contains(toolbox1,screwdriver_250mm_1).
contains(toolbox1,screwdriver_200mm_1).
contains(toolbox1,screwdriver_175mm_1).
contains(toolbox1,socket_1).
contains(toolbox1,spanner_22mm_1).

torch(torch_1).
knife(utility_knife_1).

hand(left).
hand(right).

location(l1).
location(l2).
location(l3).

fluent(at(A,L)) :- agent(A), location(L).
fluent(at(O,L)) :- object(O), location(L).
fluent(holding(A,H,O)) :- agent(A), hand(H), object(O).

action(go(L1,L2,A)) :- location(L1), location(L2), agent(A), diff(L1, L2).
action(pickup(L,A,H,O)) :- location(L), agent(A), hand(H), object(O).
action(dropoff(L,A,H,O)) :- location(L), agent(A), hand(H), object(O).
action(handover(L,A,H,O,A2,H2)) :- location(L), agent(A), hand(H), object(O), agent(A2), hand(H2), diff(A, A2).

causes(go(L1,L2,A), at(A,L2), []) :- action(go(L1,L2,A)), fluent(at(A,L2)).
causes(go(L1,L2,A), mneg(at(A,L1)), []) :- action(go(L1,L2,A)), fluent(at(A,L1)).
causes(pickup(L,A,H,O), holding(A,H,O), []) :- action(pickup(L,A,H,O)), fluent(holding(A,H,O)).
causes(pickup(L,A,H,O), mneg(at(O,L)), []) :- action(pickup(L,A,H,O)), fluent(at(O,L)).
causes(dropoff(L,A,H,O), at(O,L), []) :- action(dropoff(L,A,H,O)), fluent(at(O,L)).
causes(dropoff(L,A,H,O), mneg(holding(A,H,O)), []) :- action(dropoff(L,A,H,O)), fluent(holding(A,H,O)).
causes(handover(L,A,H,O,A2,H2), holding(A2,H2,O), []) :- action(handover(L,A,H,O,A2,H2)), fluent(holding(A2,H2,O)).
causes(handover(L,A,H,O,A2,H2), mneg(holding(A,H,O)), []) :- action(handover(L,A,H,O,A2,H2)), fluent(holding(A,H,O)).

caused([at(A,L)], mneg(at(A,L1))) :- agent(A), fluent(at(A,L)), fluent(at(A,L1)).
caused([at(O,L)], mneg(at(O,L1))) :- object(O), fluent(at(O,L)), fluent(at(O,L1)).

executable(go(L1,L2,A), [at(A,L1)]) :- action(go(L1,L2,A)).
executable(pickup(L,A,H,O), [at(A,L), at(O,L)]) :- action(pickup(L,A,H,O)).
executable(dropoff(L,A,H,O), [at(A,L), holding(A,H,O)]) :- action(dropoff(L,A,H,O)).
executable(handover(L,A,H,O,A2,H2), [at(A,L), at(A2,L), holding(A,H,O), mneg(holding(A2,H2,O2))]) :- action(handover(L,A,H,O,A2,H2)), fluent(holding(A2,H2,O2)).

initially(at(robot_1,l1)).
initially(at(operator_1,l3)).
initially(at(O,l2)) :- contains(toolbox,O).

%goal(holding(operator_1,right,pliers_1)).
goal(at(robot_1,l2)).
%:- dynamic goal/1.

