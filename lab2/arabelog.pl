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
mejor_movimiento(Estado, Jugador, _, dummy, Estado2) :-
    copy_term(Estado, Estado3), % 1. Copiar estado2 para no sobreescribirlo
    arg(1, Estado3, Tablero), % 2. Obtener tablero de estado2
    (
        arg(6, Estado3, 1) 
        ->
        copy_term(Estado3, Estado2),
        arg(1, Estado2, Tablero2),
        hacer_movimiento_1(Tablero2, Jugador)
        ;
        hay_movimiento(Estado3, Jugador),
        valor_celda(Tablero, I, J, Jugador),
        ver_adyacentes(Tablero, I, J, -, I2, J2),
        hacer_movimiento(Estado3, I, J, I2, J2, normal, Estado2)
    ). 
