%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% puzzle with 9 cells and 8 tiles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cell(1). cell(2). cell(3).
cell(4). cell(5). cell(6).
cell(7). cell(8). cell(9).

val(1). val(2). 
val(4). val(5). val(6).
val(7). val(8). val(9).

%%%% Topology of the chessboard

near(1,2). near(1,4).
near(2,1). near(2,3). near(2,5).
near(3,2). near(3,6).
near(4,1). near(4,5). near(4,7).
near(5,2). near(5,4). near(5,6). near(5,8).
near(6,3). near(6,5). near(6,9).
near(7,4). near(7,8).
near(8,5). near(8,7). near(8,9).
near(9,6). near(9,8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Action Theory
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fluent(at(X,Y)) :- val(X),cell(Y).
fluent(free(Y)) :- cell(Y).

action(move(X,Y)) :- val(X),cell(Y).

executable(move(X,Y),[at(X,Z),free(Y)]) :-
      val(X),cell(Y),cell(Z),near(Z,Y).
      
causes(move(X,Y),at(X,Y),[]) :- 
      val(X),cell(Y).
causes(move(X,Y),free(Z),[at(X,Z)]) :-
      val(X),cell(Y),cell(Z).
 
caused([at(X,Y)],neg(free(Y))) :- val(X),cell(Y).
caused([at(X,Y)],neg(at(X,Z))) :- val(X),cell(Y),cell(Z),neq(Y,Z).
caused([at(X,Y)],neg(at(W,Y))) :- val(X),val(W),cell(Y),neq(X,W).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 20 mosse random fatte a mano sul giochino di Federica
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%initially(at(1,3)). initially(at(2,6)). 
%initially(at(4,7)). initially(at(5,4)). initially(at(6,1)). 
%initially(at(7,9)). initially(at(8,2)). initially(at(9,8)).
%initially(free(5)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 15 mosse random fatte a mano sul giochino di Federica
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initially(at(1,2)). initially(at(2,3)). 
initially(at(4,7)). initially(at(5,4)). initially(at(6,1)). 
initially(at(7,5)). initially(at(8,8)). initially(at(9,9)).
initially(free(6)).

%%% completamento stato, per ASP

initially(neg(at(1,X))) :- cell(X),neq(X,2).
initially(neg(at(2,X))) :- cell(X),neq(X,3).
initially(neg(at(4,X))) :- cell(X),neq(X,7).
initially(neg(at(5,X))) :- cell(X),neq(X,4).
initially(neg(at(6,X))) :- cell(X),neq(X,1).
initially(neg(at(7,X))) :- cell(X),neq(X,5).
initially(neg(at(8,X))) :- cell(X),neq(X,8).
initially(neg(at(9,X))) :- cell(X),neq(X,9).
initially(neg(free(X))) :- cell(X),neq(X,6).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 10 mosse random fatte a mano sul giochino di Federica
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%initially(at(1,1)). initially(at(2,3)). 
%initially(at(4,8)). initially(at(5,2)). initially(at(6,9)). 
%initially(at(7,4)). initially(at(8,6)). initially(at(9,7)).
%initially(free(5)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal(at(X,X)) :- val(X).
goal(free(3)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


