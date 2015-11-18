blk(1).
blk(2).
blk(3).
blk(4).
blk(5).

fluent(on_table(X)):- blk(X).
fluent(clear(X)):- blk(X).
fluent(on(X,Y)):- blk(X), blk(Y), diff(X,Y).
fluent(space_on_table).

action(move(X,Y)):- blk(X), blk(Y), diff(X,Y).
action(to_table(X)):- blk(X).

causes(move(X,Y),clear(Z),[on(X,Z)]):- blk(X), blk(Y), blk(Z), action(move(X,Y)), diff(X,Y,Z).
causes(move(X,Y),on(X,Y),[]):- blk(X), blk(Y), action(move(X,Y)).
causes(move(X,Y),mneg(on(X,Z)),[on(X,Z)]):- blk(X), blk(Y), blk(Z), action(move(X,Y)), diff(X,Y,Z).
causes(move(X,Y),space_on_table,[on_table(X)]):- blk(X), blk(Y), action(move(X,Y)).
causes(to_table(X),on_table(X),[]):- blk(X), action(to_table(X)).
causes(to_table(X),clear(Y),[on(X,Y)]):- blk(X), blk(Y), diff(X,Y), action(to_table(X)).
causes(to_table(X),mneg(space_on_table),[on_table(Y),on_table(Z)]):- blk(X), blk(Y), blk(Z), diff(X,Y,Z), action(to_table(X)).

caused([on(X,Y)],mneg(clear(Y))) :- blk(X), blk(Y), diff(X,Y).
caused([clear(Y)],mneg(on(X,Y))) :- blk(X), blk(Y), diff(X,Y).
caused([on(X,Y)],mneg(on_table(X))) :- blk(X), blk(Y), diff(X,Y).
caused([on_table(X)],mneg(on(X,Y))) :- blk(X), blk(Y), diff(X,Y).
caused([on(X,Y)],mneg(on(Y,X))) :- blk(X), blk(Y), diff(X,Y).
executable(move(X,Y),[clear(X),clear(Y)]) :-
blk(X), blk(Y), action(move(X,Y)).
executable(to_table(X),[clear(X),mneg(on_table(X)),space_on_table]):-
blk(X), action(to_table(X)).
initially(clear(5)).
initially(mneg(clear(X))) :- blk(X), X<5.
initially(on_table(1)).
initially(mneg(on_table(X))) :- blk(X), X>1.
initially(space_on_table).
initially(on(X,Y)) :- blk(X), blk(Y), Y<5, X is Y+1.
initially(mneg(on(X,Y))):- blk(X), blk(Y), X<Y.
initially(mneg(on(X,Y))) :- blk(X), blk(Y), Y<5,
diff(Y,X), P is Y+1, diff(X,P).
goal(on(X,Y)) :- blk(X), blk(Y), Y<4, X is Y+2.
goal(on_table(1)). goal(on_table(2)).
goal(space_on_table).
