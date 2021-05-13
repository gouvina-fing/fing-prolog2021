% Tablero:
% -> Grilla 5x5: m(f(-,-,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-))
% -> Jugador X: negro
% -> Jugador O: blanco
% -> Vacío: vacio
% Estado:
% -> estado(Tablero, A, B, C, D, E)
% -> Tablero es la grilla 5x5
% -> A = Cantidad de turnos seguidos sin jugadas de negro X 
% -> B = Cantidad de turnos seguidos sin jugadas de blanco O
% -> C = Cantidad de turnos seguidos sin que negro X capture fichas
% -> D = Cantidad de turnos seguidos sin que blanco O capture fichas
% -> E = Fase del juego (1 insertando fichas, 2 jugando)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREDICADOS AUXILIARES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREDICADOS PRINCIPALES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% hay_movimiento(+Estado,+Jugador): es exitoso si hay algún movimiento posible para el jugador
hay_movimiento(Estado, Jugador).

% hay_posible_captura(+Estado, +Jugador): dado un Estado y un jugador, veo si alguno de los movimientos que puede realizar lleva a una captura
hay_posible_captura(Estado, Jugador).

% Hace el movimiento a partir del Estado inicial del tablero, las coordenadas origen y destino y el tipo de movimiento
% el tipo de movimiento puede ser normal o con_captura. En el segundo caso, el movimiento debe incluir una captura, o fallar.
% Tablero y Tablero2 tiene el mismo término, solamente se utiliza para poder consultar la variable de salida desde el bridge
% hacer_movimiento(+Tablero, +FilaOrigen,+ColumnaOrigen,+FilaDestino,+ColumnaDestino,+TipoMovimiento,-Estado2).
hacer_movimiento(Tablero, FilaOrigen, ColumnaOrigen, FilaDestino, ColumnaDestino, TipoMovimiento, Estado2).
	
% mejor_movimiento(+Estado,+Jugador,+NivelMinimax,+Estrategia,-Estado2): dado un estado, un jugador, un nivel para minimax, y una estrategia, 
% devuelve la mejor jugada posible. Estrategia es solamente un átomo que se le asigna para poder implementar más de una estrategia
mejor_movimiento(Estado, Jugador, Nivel, Estrategia, Estado2)
