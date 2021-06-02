% Jugador X va 3 turnos sin moverse -> gana O
chequear_final(estado(m(f(-,-,-,-,-),f(-,-,o,x,-),f(-,-,o,x,-),f(x,o,o,x,-),f(x,x,-,-,o)),3,0,0,0,0), R).


% Jugador O va 3 turnos sin moverse -> gana X
chequear_final(estado(m(f(-,-,-,-,-),f(-,-,o,x,-),f(-,-,o,x,-),f(x,o,o,x,-),f(x,x,-,-,o)),0,3,0,0,0), R).


% Ambos jugadores van 12 o más jugadas sin capturar -> empate 
chequear_final(estado(m(f(-,-,-,-,-),f(-,-,o,x,-),f(-,-,o,x,-),f(x,o,o,x,-),f(x,x,-,-,o)),0,0,10,10,0), R). % -> aca no deberia decir empate ni final
chequear_final(estado(m(f(-,-,-,-,-),f(-,-,o,x,-),f(-,-,o,x,-),f(x,o,o,x,-),f(x,x,-,-,o)),0,0,13,10,0), R). % -> aca no deberia decir empate ni final

% empates
chequear_final(estado(m(f(-,-,-,-,-),f(-,-,o,x,-),f(-,-,o,x,-),f(x,o,o,x,-),f(x,x,-,-,o)),0,0,12,12,0), R). 
chequear_final(estado(m(f(-,-,-,-,-),f(-,-,o,x,-),f(-,-,o,x,-),f(x,o,o,x,-),f(x,x,-,-,o)),0,0,13,13,0), R). 


% Ya no quedan piezas de alguno de los 2 jugadores

% O se quedo sin piezas, gano X
chequear_final(estado(m(f(-,-,-,-,-),f(-,-,x,x,-),f(-,-,-,x,-),f(x,x,-,x,-),f(x,x,-,-,-)),0,0,0,0,0), R).


% X se quedo sin piezas, gano O
chequear_final(estado(m(f(-,-,-,-,-),f(-,-,o,-,-),f(-,-,o,-,-),f(-,o,o,-,-),f(-,-,-,-,o)),1,0,0,0,0), R).
