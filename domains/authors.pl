% action theory

author(andy). author(ago). author(rico).
place(udine). place(laquila).
place(paris). place(lascruces).

fluent(alive(A)):-author(A).
fluent(armed(A)):-author(A).
fluent(stay(A,P)):-author(A),place(P).

action(shoot(A,P)):-author(A),place(P).
action(move(A,P)):-author(A),place(P).

causes(shoot(A,P),mneg(alive(B)),[stay(B,P)]):-
    author(A),author(B),place(P).
causes(shoot(A,P),mneg(armed(A)),[]) :-
    author(A),place(P).
causes(move(A,P),stay(A,P),[] ):-
    author(A),place(P).

caused([stay(A,P1)],mneg(stay(A,P2))):-
    author(A),place(P1),place(P2),diff(P1,P2).
caused([stay(A,P)],mneg(stay(B,P)) ):-
    author(A),author(B),place(P),diff(A,B).

executable(shoot(A,P),[armed(A),alive(A)]):-
    author(A),place(P).
executable(move(A,P2),
        [alive(A),stay(A,P1),mneg(stay(A,P2)),
        mneg(stay(B,P2)),mneg(stay(C,P2))]):-
    author(A),author(B),author(C),
    place(P1),place(P2),
    diff(P1,P2),diff(A,B,C).

initially(stay(andy,laquila)).
initially(stay(ago,udine)).
initially(stay(rico,lascruces)).
initially(armed(A)):-author(A).
initially(alive(A)):-author(A).

goal(mneg(armed(A))):-author(A).
goal(alive(rico)).
goal(stay(andy,paris)).

