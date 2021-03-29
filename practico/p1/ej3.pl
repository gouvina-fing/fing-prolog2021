nat(0).
nat(s(X)) :- nat(X).

suma(0, Y, Y).
suma(X, 0, X).

suma(s(X), Y, S) :- suma(X, s(Y), S).

resta(X, 0, X).
resta(s(X), s(Y), R) :- resta(X, Y, R).

producto(0, _, 0).
producto(_, 0, 0).

producto(s(0), Y, Y).
producto(Y, s(0), Y).

producto(s(X), Y, P) :- producto(X, Y, Z), suma(Y, Z, P). 

distintos(X, Y) :- not(resta(X, Y, 0)).

factorial(0, s(0)).
factorial(s(0), s(0)).

factorial(s(X), Y) :- factorial(X, Z), producto(s(X), Z, Y).

potencia(_, 0, s(0)).
potencia(X, s(0), X).
potencia(X, s(Y), Z) :- potencia(X, Y, Z1), producto(X, Z1, Z).