% Test de suicidio de O (1)-> Exito (No suicidio)
mejor_movimiento(((m(f(x,-,-,-,-),f(-,o,-,-,-),f(-,x,-,-,-),f(-,-,o,-,-),f(-,-,-,-,-))),0,0,0,0,0), o, 2, minimax, Estado2).
%Estado2 =  (m(f(x, -, -, -, -), f(-, o, -, -, -), f(-, -, -, -, -), f(-, o, -, -, -), f(-, -, -, -, -)), 0, 0, 0, 0, 0) .

% Test de suicidio de O (2)-> Exito (No suicidio)
mejor_movimiento(((m(f(o,x,-,o,-),f(o,x,-,-,-),f(o,x,-,o,x),f(-,-,-,-,-),f(-,-,-,x,-))),0,0,0,0,0), o, 2, minimax, Estado2).
%Estado2 =  (m(f(o, -, -, -, -), f(o, -, -, -, -), f(o, -, o, o, x), f(-, -, -, -, -), f(-, -, -, x, -)), 0, 0, 0, 0, 0) .

% Test de suicidio de O (3)-> Exito (No suicidio)
mejor_movimiento(((m(f(o,x,-,o,x),f(-,-,x,-,-),f(-,-,-,o,x),f(-,-,-,o,x),f(-,-,-,o,x))),0,0,0,0,0), o, 2, minimax, Estado2).
%Estado2 =  (m(f(o, -, o, -, x), f(-, -, -, -, -), f(-, -, o, -, x), f(-, -, -, o, x), f(-, -, -, o, x)), 0, 0, 0, 0, 0) .

% --------------------------------------------------------------------------------------------------------------------------------
% --------------------------------------------------------------------------------------------------------------------------------
% --------------------------------------------------------------------------------------------------------------------------------

Estado = estado(m(f(-,-,o,o,x),f(x,-,x,-,o),f(x,x,-,x,x),f(-,-,x,-,x),f(x,x,o,x,o)),0,0,0,2,2), mejor_movimiento(Estado, o, 3, minimax, Estado2).
% salida
% Opcion A 97777777777777775777
% Opcion B 99
% Opcion C 888877777777777777777
% Puntaje final 8
% Estado = estado(m(f(-, -, o, o, x), f(x, -, x, -, o), f(x, x, -, x, x), f(-, -, x, -, x), f(x, x, o, x, o)), 0, 0, 0, 2, 2),
% Estado2 = estado(m(f(-, -, o, o, x), f(x, -, x, o, -), f(x, x, -, x, x), f(-, -, x, -, x), f(x, x, o, x, o)), 0, 0, 0, 2, 2) 

Estado = estado(m(f(-, -, o, o, x), f(x, -, x, o, -), f(x, x, -, x, x), f(-, -, x, -, x), f(x, x, o, x, o)), 0, 0, 0, 2, 2), mejor_movimiento(Estado, x, 2, minimax, Estado2).
% salida
%  Nivel 888
 % Nivel 8
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
 % Nivel 7
% Puntaje final 8

% Estado = estado(m(f(-, -, o, o, x), f(x, -, x, o, -), f(x, x, -, x, x), f(-, -, x, -, x), f(x, x, o, x, o)), 0, 0, 0, 2, 2),
% Estado2 = estado(m(f(-, -, o, o, -), f(x, -, x, -, x), f(x, x, -, x, x), f(-, -, x, -, x), f(x, x, o, x, o)), 0, 0, 0, 2, 2)


Estado = estado(m(f(-, -, o, o, -), f(x, -, x, -, x), f(x, x, -, x, x), f(-, -, x, -, x), f(x, x, o, x, o)), 0, 0, 0, 2, 2), mejor_movimiento(Estado, o, 1, minimax, Estado2).
% salida
 % Nivel 8
 % Nivel 8
 % Nivel 8
% Puntaje final 8

% Estado = estado(m(f(-, -, o, o, -), f(x, -, x, -, x), f(x, x, -, x, x), f(-, -, x, -, x), f(x, x, o, x, o)), 0, 0, 0, 2, 2),
% Estado2 = estado(m(f(-, o, -, o, -), f(x, -, x, -, x), f(x, x, -, x, x), f(-, -, x, -, x), f(x, x, o, x, o)), 0, 0, 0, 2, 2)

Estado = estado(m(f(-, o, -, o, -), f(x, -, x, -, x), f(x, x, -, x, x), f(-, -, x, -, x), f(x, x, o, x, o)), 0, 0, 0, 2, 2), mejor_movimiento(Estado, x, 0, minimax, Estado2).
% salida
% 8
% Estado = Estado2, Estado2 = estado(m(f(-, o, -, o, -), f(x, -, x, -, x), f(x, x, -, x, x), f(-, -, x, -, x), f(x, x, o, x, o)), 0, 0, 0, 2, 2).
