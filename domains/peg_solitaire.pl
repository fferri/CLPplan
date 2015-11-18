%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PEG SOLITAIRE
%%% csplib #037 - see www.csplib.org
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coded by Andrea Schiavinato -
%%%      Univ of Udine -Sept 2008
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% It needs 31 actions.
%%% Call  :- sicsplan(31).
%%% (runtime ~50s with 2GHz)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Chessboard:

                      cell(0,2). cell(0,3). cell(0,4).
                      cell(1,2). cell(1,3). cell(1,4).
cell(2,0). cell(2,1). cell(2,2). cell(2,3). cell(2,4). cell(2,5). cell(2,6).
cell(3,0). cell(3,1). cell(3,2). cell(3,3). cell(3,4). cell(3,5). cell(3,6).
cell(4,0). cell(4,1). cell(4,2). cell(4,3). cell(4,4). cell(4,5). cell(4,6).
                      cell(5,2). cell(5,3). cell(5,4).
                      cell(6,2). cell(6,3). cell(6,4).

%%% Directions

direction(north).
direction(south).
direction(east).
direction(west).

%%% Cell Neighbourough

close(X,Y, north, X,Y1) :-   Y1 is Y-1.
close(X,Y, south, X,Y1) :-   Y1 is Y+1.
close(X,Y, east,  X1,Y) :-   X1 is X+1.
close(X,Y, west,  X1,Y) :-   X1 is X-1.

jump(X,Y, north, X,Y2) :-   Y2 is Y-2.
jump(X,Y, south, X,Y2) :-   Y2 is Y+2.
jump(X,Y, east,  X2,Y) :-   X2 is X+2.
jump(X,Y, west,  X2,Y) :-   X2 is X-2.

%%%%%%%% FLUENT %%%%%%%%

fluent( free(X,Y) ) :- cell(X,Y).

%%%%%%%% MOVE ACTION %%%%%%%%

action(move(X,Y,Dir)) :-
    cell(X,Y),
    direction(Dir),
    jump(X,Y,Dir,X2,Y2),
    cell(X2,Y2).

%%%%%%%% EXECUTABILITY %%%%%%%%

executable(move(X,Y,Dir), [neg(free(X,Y)),neg(free(X1,Y1)),free(X2, Y2)]):-
    action(move(X,Y,Dir)),
    close(X,Y,Dir, X1,Y1),
    jump(X,Y,Dir, X2,Y2).

%%%%%%% EFFECTS

causes(move(X,Y,Dir), free(X,Y), []) :-
    action(move(X,Y,Dir)).

causes(move(X,Y,Dir), free(X1,Y1), []) :-
    action(move(X,Y,Dir)),
    close(X,Y,Dir,X1,Y1).

causes(move(X,Y,Dir), neg(free(X2,Y2)), []) :-
    action(move(X,Y,Dir)),
    jump(X,Y,Dir,X2,Y2).

%%%%%%%% Initial State and Goal

initially(free(3,3)).
initially(neg(free(X,Y))) :-
    cell(X,Y),
    neq_pair(X,Y,3,3).

goal(neg(free(3,3))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neq_pair(X1,_Y1,X2,_Y2) :-
   neq(X1,X2).
neq_pair(_X1,Y1,_X2,Y2) :-
   neq(Y1,Y2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
