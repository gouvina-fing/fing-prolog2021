distintos(X, Y) :- X \= Y. 

progenitor(carla, pedro).
progenitor(carla, ana).
progenitor(juan, pedro).
progenitor(juan, ana).

progenitor(pedro, julia).
progenitor(pedro, julio).

progenitor(ana, pepe).
progenitor(ana, jime).

casados(carla, juan).
casados(juan, carla).
casados(sofia, pedro).
casados(pedro, sofia).
casados(ana, jose).
casados(jose, ana).




hermanos(X, Y) :-  progenitor(Z,X), progenitor(Z, Y), distintos(X, Y).

tio(X, Y) :- hermanos(X, Z), progenitor(Z, Y), distintos(X, Y).

tio_politico(X, Y) :- casados(X, Z), tio(Z, Y).

cunado(X, Y) :- hermanos(X, Z), casados(Z, Y), distintos(X, Y).
cunado(X, Y) :- hermanos(Y, Z), casados(Z, X), distintos(X, Y).

con_cunado(X, Y) :- casados(X, Z), cunado(Z, Y), distintos(X, Y), not(hermanos(X,Y)).