% Predicado ver_adyacentes
% -------------------------------------------------------------------------------------------------------------------------

% Caso exitoso -> 4 adyacentes
ver_adyacentes(m(f(-,x,-,-,-),f(x,-,x,-,-),f(-,x,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-)), 2, 2, x, M, N).
% Caso exitoso -> 3 adyacentes
ver_adyacentes(m(f(-,x,-,-,-),f(x,-,x,-,-),f(-,o,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-)), 2, 2, x, M, N).
% Caso exitoso -> 2 adyacentes
ver_adyacentes(m(f(-,x,-,-,-),f(x,-,-,-,-),f(-,o,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-)), 2, 2, x, M, N).
% Caso exitoso -> 1 adyacente
ver_adyacentes(m(f(-,-,-,-,-),f(x,-,-,-,-),f(-,o,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-)), 2, 2, x, M, N).
% Caso fallido
ver_adyacentes(m(f(-,-,-,-,-),f(o,-,-,-,-),f(-,o,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-)), 2, 2, x, M, N).

% Predicado hay_posible_captura
% -------------------------------------------------------------------------------------------------------------------------

% Caso exitoso -> 3 posibilidades
hay_posible_captura((m(f(-,-,-,-,-),f(-,-,x,-,-),f(-,x,-,o,x),f(-,-,x,-,-),f(-,-,-,-,-)),0,0,0,0,0), x).
% Caso exitoso -> 2 posibilidades
hay_posible_captura((m(f(-,-,-,-,-),f(-,-,-,-,-),f(-,x,-,o,x),f(-,-,x,-,-),f(-,-,-,-,-)),0,0,0,0,0), x).
% Caso exitoso -> 1 posibilidad
hay_posible_captura((m(f(x,x,o,o,x),f(x,x,-,x,o),f(o,-,-,-,x),f(o,x,x,x,o),f(o,x,x,o,o)),0,0,0,0,0), x).
% Caso fallido
hay_posible_captura((m(f(-,-,-,-,-),f(-,-,-,-,-),f(-,-,-,o,x),f(-,-,o,-,-),f(-,-,-,-,-)),0,0,0,0,0), x).