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