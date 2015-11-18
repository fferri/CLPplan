% Hanoi problem with N blocks 
% 2^N - 1 moves are needed

% The number of blocks N is n(N).
n(4).

block(X) :- n(N), between(1,N,X).  
floor(0).

rod(a).
rod(b).
rod(c).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fluent(in_rod(X,P)) :- block(X),rod(P).

fluent(top(P,Y)) :- rod(P), (block(Y) ; floor(Y)).

fluent(on(X,Y)):- block(X), block(Y), X < Y.
fluent(on(X,0)):- block(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(move(X,A,B)):- block(X), rod(A), rod(B), diff(A,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

executable(move(X,A,B), [top(A,X),top(B,Z)]) :-  
      action(move(X,A,B)),
      block(X),block(Z), X < Z.

executable(move(X,A,B), [top(A,X),top(B,0)]) :-  
       action(move(X,A,B)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

causes(move(X,A,B),in_rod(X,B),[]):- 
       action(move(X,A,B)).
causes(move(X,A,B),on(X,Z),[top(B,Z)]):- 
       (block(Z) ; floor(Z)),
       action(move(X,A,B)).
causes(move(X,A,B), top(B,X),[]):- 
       action(move(X,A,B)).
causes(move(X,A,B), top(A,Y),[on(X,Y)]):- 
       fluent(on(X,Y)),
       action(move(X,A,B)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

caused([on(X,Y)],mneg(on(X,Z))) :- 
      fluent(on(X,Y)),
      fluent(on(X,Z)), 
      diff(X,Y,Z).
caused([on(X,Y)],mneg(on(Z,Y))) :- 
      fluent(on(X,Y)),
      fluent(on(Z,Y)), 
      block(Y), %%% Per 0 potrebbe andar bene
      diff(X,Y,Z).
      
caused([in_rod(X,P)],mneg(in_rod(X,Q))) :-
   block(X),rod(P),rod(Q),diff(P,Q).

caused([top(X,A)],mneg(top(X,B))) :- 
      fluent(top(X,A)),
      fluent(top(X,B)),
      diff(A,B).
caused([top(X,A)],mneg(top(Y,A))) :- 
      fluent(top(X,A)),
      fluent(top(Y,A)),
      diff(X,Y),
      block(A). %%% A=0 potrebbe andar bene

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initially(in_rod(A,a)):- block(A).
initially(top(a,1)).
initially(top(b,0)).
initially(top(c,0)).

%%%% Superfluo: lo capisce da solo
initially(on(N,0)):-  n(N).
initially(on(X,Y)):-  block(X),(block(Y) ; floor(Y)),n(N), X < N, Y is X + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simple goal:
%goal(in_rod(A,b)):- block(A), n(N), A < N/2.

% classic goal - solve completely:
goal(in_rod(A,b)):- block(A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




