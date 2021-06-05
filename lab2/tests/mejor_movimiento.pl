% Test de suicidio de O (1)-> Exito (No suicidio)
mejor_movimiento(((m(f(x,-,-,-,-),f(-,o,-,-,-),f(-,x,-,-,-),f(-,-,o,-,-),f(-,-,-,-,-))),0,0,0,0,0), o, 2, minimax, Estado2).
%Estado2 =  (m(f(x, -, -, -, -), f(-, o, -, -, -), f(-, -, -, -, -), f(-, o, -, -, -), f(-, -, -, -, -)), 0, 0, 0, 0, 0) .

% Test de suicidio de O (2)-> Exito (No suicidio)
mejor_movimiento(((m(f(o,x,-,o,-),f(o,x,-,-,-),f(o,x,-,o,x),f(-,-,-,-,-),f(-,-,-,x,-))),0,0,0,0,0), o, 2, minimax, Estado2).
%Estado2 =  (m(f(o, -, -, -, -), f(o, -, -, -, -), f(o, -, o, o, x), f(-, -, -, -, -), f(-, -, -, x, -)), 0, 0, 0, 0, 0) .

% Test de suicidio de O (3)-> Exito (No suicidio)
mejor_movimiento(((m(f(o,x,-,o,x),f(-,-,x,-,-),f(-,-,-,o,x),f(-,-,-,o,x),f(-,-,-,o,x))),0,0,0,0,0), o, 2, minimax, Estado2).
%Estado2 =  (m(f(o, -, o, -, x), f(-, -, -, -, -), f(-, -, o, -, x), f(-, -, -, o, x), f(-, -, -, o, x)), 0, 0, 0, 0, 0) .