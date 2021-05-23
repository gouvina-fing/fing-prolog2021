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