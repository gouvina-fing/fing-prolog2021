:- use_module(core).
:- use_module(ai).

% Tablero:
% -> Grilla 5x5: m(f(-,-,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-),f(-,-,-,-,-))
% -> Jugador X: x
% -> Jugador O: o
% -> Vacío: -
% Estado:
% -> estado(Tablero, A, B, C, D, E)
% -> Tablero es la grilla 5x5
% -> A = Cantidad de turnos seguidos sin jugadas de negro X 
% -> B = Cantidad de turnos seguidos sin jugadas de blanco O
% -> C = Cantidad de turnos seguidos sin que negro X capture fichas
% -> D = Cantidad de turnos seguidos sin que blanco O capture fichas
% -> E = Fase del juego (1 insertando fichas, 2 jugando)

%% PREDICADOS DE JUEGO
%% ----------------------------------------------------------------------------------------------------------------------------------------------

% hay_movimiento(+Estado,+Jugador) -> es exitoso si hay algún movimiento posible para el jugador
% dos caminos:
% - agarro todas mis piezas y busco si alguna tiene una casilla vacia a la que ir
% - agarro todas las casillas vacias y veo si alrededor de ella hay una pieza mia
hay_movimiento(Estado, Jugador) :-
    arg(1, Estado, Tablero),
    valor_celda(Tablero, I, J, Jugador),
    hay_movimiento_celda(Tablero, I, J),
    !.

% hay_posible_captura(+Estado, +Jugador) -> dado un Estado y un jugador, veo si alguno de los movimientos que puede realizar lleva a una captura
hay_posible_captura(Estado, Jugador):-
    arg(1, Estado, Tablero),
    jugador_opuesto(Jugador, JugadorOpuesto),
    valor_celda(Tablero, I, J, JugadorOpuesto),
    hay_posible_captura_celda(Tablero, I, J, Jugador),
    !.

% hacer_movimiento((m(f(-,-,-,-,-),f(-,-,-,x,-),f(-,x,o,-,-),f(-,-,-,o,-),f(-,-,-,x,-)),0,0,0,0,2),2,4,3,4,normal,E).
% Hace el movimiento a partir del Estado inicial del tablero, las coordenadas origen y destino y el tipo de movimiento
% el tipo de movimiento puede ser normal o con_captura. En el segundo caso, el movimiento debe incluir una captura, o fallar.
% Estado y Estado2 tienen el mismo término, solamente se utiliza para poder consultar la variable de salida desde el bridge
% hacer_movimiento(+Estado, +FilaOrigen,+ColumnaOrigen,+FilaDestino,+ColumnaDestino,+TipoMovimiento,-Estado2).
hacer_movimiento(Estado, FilaOrigen, ColumnaOrigen, FilaDestino, ColumnaDestino, TipoMovimiento, Estado2) :-
    % Validación de movimiento
    arg(1, Estado, Tablero), % 1. Obtener tablero
    valor_celda(Tablero, FilaOrigen, ColumnaOrigen, Jugador), % 2. Obtener valor de casilla origen
    (Jugador = x ; Jugador = o), % 3. Chequear que la casilla origen no está vacía
    valor_celda(Tablero, FilaDestino, ColumnaDestino, -), % 4. Chequear que la casilla destino esta vacía
    (TipoMovimiento = con_captura -> hay_posible_captura(Estado, Jugador) ; true), % 5. Chequear que haya captura posible si el movimiento es con_captura
    % Realización de movimiento
    copy_term(Estado, Estado2), % 1. Copiar estado2 para no sobreescribirlo
    arg(1, Estado2, Tablero), % 2. Obtener tablero de estado2
    modificar_celda(Tablero, FilaOrigen, ColumnaOrigen, -), % 3. Vaciar celda origen
    modificar_celda(Tablero, FilaDestino, ColumnaDestino, Jugador), % 4. Sobreescribir celda destino
    % Realización de captura(s)
    jugador_opuesto(Jugador, JugadorOpuesto),
    (capturar_norte(Tablero, FilaDestino, ColumnaDestino, Jugador, JugadorOpuesto, FilaCapturada1, Exito) -> modificar_celda(Tablero, FilaCapturada1, ColumnaDestino, -) ; true),
    (capturar_sur(Tablero, FilaDestino, ColumnaDestino, Jugador, JugadorOpuesto, FilaCapturada2, Exito) -> modificar_celda(Tablero, FilaCapturada2, ColumnaDestino, -) ; true),
    (capturar_oeste(Tablero, FilaDestino, ColumnaDestino, Jugador, JugadorOpuesto, ColumnaCapturada1, Exito) -> modificar_celda(Tablero, FilaDestino, ColumnaCapturada1, -) ; true),
    (capturar_este(Tablero, FilaDestino, ColumnaDestino, Jugador, JugadorOpuesto, ColumnaCapturada2, Exito) -> modificar_celda(Tablero, FilaDestino, ColumnaCapturada2, -) ; true),
    (TipoMovimiento = con_captura -> Exito == exito; true).

%% PREDICADOS DE IA
%% ----------------------------------------------------------------------------------------------------------------------------------------------

% mejor_movimiento(+Estado,+Jugador,+NivelMinimax, +Estrategia, -Estado2) -> dado un estado, un jugador, un nivel para minimax, y una estrategia, 
% devuelve la mejor jugada posible. Estrategia es solamente un átomo que se le asigna para poder implementar más de una estrategia
% VERSION DUMMY
% -> Fase 1: Rellena los primeros dos lugares libres que encuentra
% -> Fase 2: Mueve la primer pieza con movimiento posible y si captura, pasa
mejor_movimiento(Estado, Jugador, _, dummy, Estado2) :-
    copy_term(Estado, Estado2), % 1. Copiar estado para no editar tablero original
    arg(1, Estado2, Tablero), % 2. Obtener tablero del estado
    (arg(6, Estado2, 1) % 3. Chequear fase
        -> % Si fase 1
            hacer_movimiento_fase1(Tablero, Jugador, dummy) % 3.1. Versión dummy fase 1; elige los primeros 2 lugares libres
        ; % Si fase 2
            hay_movimiento(Estado2, Jugador), % 3.2.1. Comprueba que haya movimientos posibles
            valor_celda(Tablero, I, J, Jugador), % 3.2.2. Unifica con todas las piezas del jugador
            ver_adyacentes(Tablero, I, J, -, I2, J2), % 3.2.3. Para cada pieza comprueba si hay celdas vacías adyacentes
            hacer_movimiento(Estado2, I, J, I2, J2, normal, Estado2) % 3.2.4. Para la primer celda vacía adyacente, realiza el movimiento
    ).

% VERSION MINIMAX
mejor_movimiento(Estado, Jugador, Nivel, minimax, Estado2) :-
    copy_term(Estado, EstadoAux), % 1. Copiar estado para no editar tablero original
    arg(1, EstadoAux, Tablero), % 2. Obtener tablero del estado
    (arg(6, EstadoAux, 1) % 3. Chequear fase
        -> % Si fase 1
            hacer_movimiento_fase1(Tablero, Jugador, dummy), % 3.1. Versión dummy fase 1; elige los primeros 2 lugares libres
            EstadoAux = Estado2
        ; % Si fase 2
            pre_minimax(Alpha, Beta),
            minimax(Nivel, Alpha, Beta, Jugador, EstadoAux, Estado2, _)
    ).
