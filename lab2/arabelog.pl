% El tablero es un damero de 5 x 5. Cada celda tiene el valor negra, blanca, o vacía.
% Vamos a utilizar, por razones de eficiencia, matrices representadas como functores en Prolog (Pr�ctico 8)
% Un tablero tiene la forma 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREDICADOS AUXILIARES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREDICADOS QUE SE INVOCAN DESDE EL BRIDGE, NECESARIAMENTE DEBEN IMPLEMENTARSE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Hace el movimiento a partir del Estado inicial del tablero, las coordenadas origen y destino y el tipo de movimiento
% el tipo de movimiento puede ser normal o con_captura. En el segundo caso, el movimiento debe incluir una captura, o fallar.
% Tablero y Tablero2 tiene el mismo término, solamente se utiliza para poder consultar la variable de salida desde el bridge
% hacer_movimiento(+Tablero, +FilaOrigen,+ColumnaOrigen,+FilaDestino,+ColumnaDestino,+TipoMovimiento,-Estado2).
hacer_movimiento(Tablero, FilaOrigen, ColumnaOrigen, FilaDestino, ColumnaDestino, TipoMovimiento, Estado2).

% hay_posible_captura(+Estado, +Jugador): dado un Estado y un jugador, veo si alguno de los movimientos que puede realizar lleva a una captura
hay_posible_captura(Estado, Jugador).

% hay_movimiento(+Estado,+Jugador): es exitoso si hay algún movimiento posible para el jugador
hay_movimiento(Estado, Jugador).
	
% mejor_movimiento(+Estado,+Jugador,+NivelMinimax,+Estrategia,-Estado2): dado un estado, un jugador, un nivel para minimax, y una estrategia, 
% devuelve la mejor jugada posible. Estrategia es solamente un átomo que se le asigna para poder implementar más de una estrategia
mejor_movimiento(Estado, Jugador, N, Estrategia, Estado2)
