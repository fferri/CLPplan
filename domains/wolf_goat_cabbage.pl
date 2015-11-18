%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% CAPRA E CAVOLO IN B (Boolean Fluents --- SICStus 3 e SICStus4)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% :-sicsplan(23).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ogg(capra). ogg(cavolo). ogg(lupo). ogg(uomo).

lato(sin). lato(des).

pos(X) :- lato(X).
pos(barca).

%%%%%%%%%%%%%%%%%%%%%

fluent(sta_in(X,Y)) :- ogg(X), pos(Y).
fluent(barca_in(Y)) :- lato(Y).
fluent(vivi).

%%%%%%%%%%%%%%%%

action(naviga(A,B)) :-  lato(A),lato(B), neq(A,B).
action(sali(A)) :- ogg(A).
action(scendi(A)) :- ogg(A).

%%%%%%%%%%%%%%%

executable(naviga(A,B),[barca_in(A),sta_in(uomo,barca)]) :-
       lato(A),lato(B), neq(A,B). %%% action(naviga(A,B)).
executable(sali(A),[barca_in(L),sta_in(A,L)]) :-
       ogg(A),lato(L).
executable(scendi(A),[sta_in(A,barca)]) :-
       ogg(A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

causes(naviga(A,B), barca_in(B), []):-
    lato(A),lato(B),neq(A,B).
causes(sali(A), sta_in(A,barca),[]) :-
    ogg(A).
causes(scendi(A), sta_in(A,L),[barca_in(L)]) :-
    ogg(A),lato(L).
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         

initially(sta_in(A,sin)) :- ogg(A).
initially(barca_in(sin)).
initially( vivi ).

%%%%%%%%%%%%%%%

goal(sta_in(A,des)) :- ogg(A).
goal( vivi ).

%%%% UNIVOCITA' POSIZIONE  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         

caused([sta_in(Ogg,L1)],neg(sta_in(Ogg,L2))) :-
      ogg(Ogg),pos(L1),pos(L2),neq(L1,L2).  
caused([barca_in(L1)],neg(barca_in(L2))) :-
      lato(L1),lato(L2),neq(L1,L2). 

%%%% Al piu' uno in barca (oltre al pastore)  %%%%%%%%%%%%%%%%%%%      

caused([sta_in(A,barca),sta_in(B,barca)],neg(vivi)) :-
      ogg(A),ogg(B),diff(A,B,uomo).

%%% Mai soli capra e cavolo O lupo e capra

caused([sta_in(lupo,L),sta_in(capra,L),neg(sta_in(uomo,L))],neg(vivi)) :-
      pos(L).
caused([sta_in(cavolo,L),sta_in(capra,L),neg(sta_in(uomo,L))],neg(vivi)) :-
      pos(L).

      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         


