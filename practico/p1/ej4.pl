% --------------------------
% Ejercicio 4
% --------------------------

% Auxiliares
% --------------------------

nat(0).
nat(s(X)) :- nat(X).

% Principales - Parte a
% --------------------------

largo([], 0).
largo([_|T], s(N)) :- largo(T, N).

ultimo([X], X).
ultimo([_|T], X) :- ultimo(T, X).

sin_ultimo([_], []).
sin_ultimo([H|T], [H|Y]) :- sin_ultimo(T, Y).

reverso([],[]).
reverso([X], [X]).
reverso(L, [Y|Z]) :- sin_ultimo(L, X), ultimo(L, Y), reverso(X, Z).

subsecuencia([], []). % Vacia e igual
subsecuencia([X], [X]). % 1 elemento e igual
subsecuencia([H|T], [H|T]) :- largo(T,s(N)), nat(N). % 2 elementos o mas e igual
subsecuencia(L, X) :- sin_ultimo(L, X). % sin ultimo
subsecuencia([_|T], X) :- subsecuencia(T, X).
subsecuencia([H|T], [H|X]) :- subsecuencia(T, X).

prefijo([], []).
prefijo([H|T], [H|T]).
prefijo([H|T],Y) :- sin_ultimo([H|T], X), prefijo(X, Y).

sufijo([], []).
sufijo([H|T], [H|T]).
sufijo([_|T], X) :- sufijo(T, X).

sublista(L, Y) :- sufijo(L, X), prefijo(X, Y).
%sublista([], []).
%sublista([H|T], [H|T]).
%sublista([_|T], X) :- sublista(T, X).
%sublista([H|T],Y) :- sin_ultimo([H|T], X), sublista(X, Y).

borrar_todos([], _, []).
borrar_todos([X], X, []).
borrar_todos([X|T], X, Y) :- borrar_todos(T, X, Y).
borrar_todos([H|T], X, [H|Y]) :- H \= X, borrar_todos(T, X, Y).

sin_repetidos([], []).
sin_repetidos([H|T], [H|Y]) :- borrar_todos(T, H, X), sin_repetidos(X,Y).

% Principales - Parte b
% --------------------------

conjunto([]).
conjunto([H|T]) :- nat(H), conjunto(T), sin_repetidos([H|T],[H|T]).

conj_iguales([], []).
conj_iguales([H1|T1], [H2|T2]) :- 
    conjunto([H2|T2]),
    borrar_todos([H2|T2], H1, X2),
    conj_iguales(T1, X2).