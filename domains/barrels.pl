%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%         Change the value of max_lit:        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 8  (=>5, 3) - solution with 8 states (7 actions)
%%% 12 (=> 7, 5 standard) - solution with 12 states 
%%% 16 (=> 9, 7) - solution with 16 states
%%% 20 (=> 11, 9)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_lit(12).  

litri(X) :-
  max_lit(N),
  interval(X,0,N).

botte(M) :- max_lit(N), M is N//2 - 1.
botte(M) :- max_lit(N), M is N//2 + 1.
botte(N) :- max_lit(N).

%%%%%%%%%%%%%%%%%%%%%

fluent(contiene(B,L)):- botte(B),litri(L),L =< B.

%%%%%%%%%%%%%%%%

action(versa(X,Y)):-  botte(X),botte(Y), neq(X,Y). 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

executable(versa(X,Y),[contiene(X,LX),contiene(Y,LY)]) :-  
        action(versa(X,Y)),
        fluent(contiene(X,LX)),
        fluent(contiene(Y,LY)),
        LX > 0,
        LY < Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

causes(versa(X,Y), contiene(X,0), [contiene(X,LX),contiene(Y,LY)]):-
        action(versa(X,Y)),
        fluent(contiene(X,LX)),
        fluent(contiene(Y,LY)),
        Y-LY >= LX.
causes(versa(X,Y), contiene(Y,LYnew), [contiene(X,LX),contiene(Y,LY)]):-
        action(versa(X,Y)),
        fluent(contiene(X,LX)),
        fluent(contiene(Y,LY)),
        Y-LY >= LX,
        LYnew is LX + LY.
causes(versa(X,Y), contiene(X,LXnew), [contiene(X,LX),contiene(Y,LY)]):-
        action(versa(X,Y)),
        fluent(contiene(X,LX)),
        fluent(contiene(Y,LY)),
        LX >= Y-LY,
        LXnew is LX-Y+LY.
causes(versa(X,Y), contiene(Y,Y), [contiene(X,LX),contiene(Y,LY)]):-
        action(versa(X,Y)),
        fluent(contiene(X,LX)),
        fluent(contiene(Y,LY)),
        LX >= Y-LY.
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
caused([ contiene(X,LX) ], neg(contiene(X,LY)) ) :-
    fluent(contiene(X,LX)),
    fluent(contiene(X,LY)),
    neq(LX,LY).
           
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         

initially(contiene(N,N)) :-
    max_lit(N).
initially(contiene(M,0)) :-
    botte(M), max_lit(N), neq(M,N).
   
%%%%%%%%%%%%%%%

goal(contiene(N,M)) :- max_lit(N), M is N//2.
goal(contiene(P,M)) :- max_lit(N), M is N//2, P is M + 1.
goal(contiene(M,0)) :- max_lit(N), M is N//2 - 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         

