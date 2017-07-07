female(a).
female(b).
male(c).
friend(b,c).
married(X,Y) :- female(X), friend(X,Y).
sad(a,happy(b,c)).
is(X).
