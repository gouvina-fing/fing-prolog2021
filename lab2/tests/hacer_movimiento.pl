% Predicado hacer_movimiento
% -------------------------------------------------------------------------------------------------------------------------

% Caso exitoso -> Movimiento normal sin captura, X (2,4)->(3,4)
hacer_movimiento((m(f(-,-,-,-,-),f(-,-,-,x,-),f(-,x,-,-,-),f(-,-,-,-,-),f(-,-,-,x,-)),0,0,0,0,2),2,4,3,4,normal,E).
% Caso exitoso -> Movimiento normal con 1 captura, X (2,4)->(3,4)
hacer_movimiento((m(f(-,-,-,-,-),f(-,-,-,x,-),f(-,x,o,-,-),f(-,-,-,-,-),f(-,-,-,x,-)),0,0,0,0,2),2,4,3,4,normal,E).
% Caso exitoso -> Movimiento normal con 2 capturas, X (2,4)->(3,4)
hacer_movimiento((m(f(-,-,-,-,-),f(-,-,-,x,-),f(-,x,o,-,-),f(-,-,-,o,-),f(-,-,-,x,-)),0,0,0,0,2),2,4,3,4,normal,E).
% Caso exitoso -> Movimiento normal con 3 capturas, X (3,4)->(3,3)
hacer_movimiento((m(f(-,-,x,-,-),f(-,-,o,-,-),f(x,o,-,x,-),f(-,-,o,-,-),f(-,-,x,-,-)),0,0,0,0,2),3,4,3,3,normal,E).

% Caso fallido -> Celda origen vacía
hacer_movimiento((m(f(-,-,-,-,-),f(-,-,-,x,-),f(-,x,-,-,-),f(-,-,-,-,-),f(-,-,-,x,-)),0,0,0,0,2),1,1,1,2,normal,E).
% Caso fallido -> Celda destino ocupada
hacer_movimiento((m(f(-,-,-,-,-),f(-,-,o,x,-),f(-,-,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-)),0,0,0,0,2),3,3,3,4,normal,E).
% Caso fallido -> Movimiento con_captura sin captura posible, X (2,4)->(3,4)
hacer_movimiento((m(f(-,-,-,-,-),f(-,-,-,x,-),f(-,x,-,-,-),f(-,-,-,-,-),f(-,-,-,x,-)),0,0,0,0,2),2,4,3,4,con_captura,E).
% Caso fallido -> Movimiento con_captura con captura posible pero no hecha, X (2,4)->(3,4)
hacer_movimiento((m(f(-,-,-,-,-),f(x,o,-,x,-),f(-,-,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-)),0,0,0,0,2),2,4,3,4,con_captura,E).

% Estados:
% hacer_movimiento((m(f(x,x,o,o,x),f(x,o,o,x,x),f(o,o,-,x,x),f(o,o,x,x,o),f(o,x,x,o,o)),0,0,0,0,2),3,2,3,3,normal,E).
% hacer_movimiento((m(f(-,-,-,-,-),f(-,-,o,x,-),f(-,-,o,x,-),f(x,o,o,x,-),f(x,x,-,-,o)),0,0,0,0,2),3,2,3,3,normal,E).

% MAL
(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, -, o, x, -), f(x, o, o, x, -), f(x, x, -, -, o)), 0, 0, 0, 0, 2) -> % INICIAL
(m(f(-, -, o, -, -), f(-, -, -, x, -), f(-, -, o, -, o), f(x, o, o, -, -), f(x, x, -, -, -)), 0, 0, 0, 0, 2),
%(m(f(-, -, -, -, -), f(-, o, -, x, -), f(-, -, o, -, o), f(x, o, o, -, -), f(x, x, -, -, -)), 0, 0, 0, 0, 2),
%(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, o, -, x, -), f(x, o, o, -, o), f(x, x, -, -, -)), 0, 0, 0, 0, 2),
%(m(f(-, -, -, -, -), f(-, -, o, -, o), f(-, o, o, -, -), f(x, -, o, -, -), f(x, x, -, -, -)), 0, 0, 0, 0, 2),
%(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, -, o, x, -), f(x, o, -, x, -), f(x, x, o, -, o)), 0, 0, 0, 0, 2),
(m(f(-, -, -, -, -), f(-, -, o, -, o), f(-, -, o, -, -), f(x, o, o, -, -), f(x, x, -, -, -)), 0, 0, 0, 0, 2),
%(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, -, o, x, -), f(x, o, o, x, -), f(x, x, -, o, -)), 0, 0, 0, 0, 2),

% BIEN
(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, -, o, x, -), f(x, o, o, x, -), f(x, x, -, -, o)), 0, 0, 0, 0, 2) -> % INICIAL
(m(f(-, -, o, -, -), f(-, -, -, x, -), f(-, -, o, x, -), f(x, o, o, x, -), f(x, x, -, -, o)), 0, 0, 0, 0, 2), % 1 - 1° Arriba
(m(f(-, -, -, -, -), f(-, o, -, x, -), f(-, -, o, x, -), f(x, o, o, x, -), f(x, x, -, -, o)), 0, 0, 0, 0, 2), % 2 - 1° Izq
(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, o, -, x, -), f(x, o, o, x, -), f(x, x, -, -, o)), 0, 0, 0, 0, 2), % 3 - 2° Izq
(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, o, o, x, -), f(x, -, o, x, -), f(x, x, -, -, o)), 0, 0, 0, 0, 2), % 4 - 3° Arriba
(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, -, o, x, -), f(x, o, -, x, -), f(x, x, o, -, o)), 0, 0, 0, 0, 2), % 5 - 4° Abajo
(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, -, o, x, -), f(x, o, o, -, o), f(x, x, -, -, -)), 0, 0, 0, 0, 2), % 6 - 5° Arriba (Captura)
(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, -, o, x, -), f(x, o, o, x, -), f(x, x, -, o, -)), 0, 0, 0, 0, 2), % 7 - 5° Izq

(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, -, o, x, -), f(x, o, o, x, -), f(x, x, -, -, o)), 0, 0, 0, 0, 2) % INICIAL
(m(f(-, -, -, -, -), f(-, -, o, x, -), f(-, -, o, x, -), f(x, o, o, -, o), f(x, x, -, -, -)), 0, 0, 0, 0, 2) % INICIAL CAPTURADO
(m(f(-, -, -, -, -), f(-, -, o, -, o), f(-, -, o, -, -), f(x, o, o, -, -), f(x, x, -, -, -)), 0, 0, 0, 0, 2) % FINAL CAPTURADO
