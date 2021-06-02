:- module(core,
[
    % GENERALES
    es_centro/2,
    jugador_opuesto/2,
    valor_celda/4,
    modificar_celda/4,
    ver_adyacentes/6,
    % HAY_MOVIMIENTO
    hay_movimiento_celda/3,
    % HAY_POSIBLE_CAPTURA
    hay_posible_captura_celda/4,
    % HACER_MOVIMIENTO
    capturar_norte/7,
    capturar_sur/7,
    capturar_oeste/7,
    capturar_este/7,
    % MINIMAX
    contar_piezas/3,
    chequear_final/2,
    actualizar_sin_movimiento/3,
    calcular_posibles_estados/3,
    hacer_movimiento_aux/8
]).

%% PREDICADOS GENERALES
%% ----------------------------------------------------------------------------------------------------------------------------------------------

% es_centro(?I, ?J) -> Dado un par de coordenadas I,J determina si son el centro del tablero o no
es_centro(3, 3).

% jugador_opuesto(?Jugador, ?JugadorOpuesto) -> Dado un tipo de Jugador, da el otro tipo de jugador en JugadorOpuesto
jugador_opuesto(o, x).
jugador_opuesto(x, o).

% valor_celda(+Tablero, ?I, ?J, ?Valor) -> Dada una matriz functor y coordenadas I y J, devuelve el valor de dicha celda
valor_celda(Tablero, I, J, Valor) :-
    arg(I, Tablero, Fila),
    arg(J, Fila, Valor).

% modificar_celda(+Tablero, +I, +J, +Valor) -> Dada una matriz functor, coordenadas I y J, y un Valor, devuelve la matriz resultante
% de sustituir el valor en la celda (I,J) con el valor Valor (modifica la matriz original)
modificar_celda(Tablero, I, J, Valor) :-
    arg(I, Tablero, Fila),
    setarg(J, Fila, Valor),
    setarg(I, Tablero, Fila).

% ver_adyacentes(Tablero, I, J, Jugador, M, N) -> Dada una matriz revisa si la pieza en coordenadas I,J es adyacente a una pieza
% del jugador Jugador y da sus coordenadas en M,N.
ver_adyacentes(Tablero, I, J, Jugador, I, J2):- J2 is J-1, valor_celda(Tablero, I, J2, Jugador). % misma fila, columna izquierda
ver_adyacentes(Tablero, I, J, Jugador, I, J3):- J3 is J+1, valor_celda(Tablero, I, J3, Jugador). % misma fila, columna derecha
ver_adyacentes(Tablero, I, J, Jugador, I2, J):- I2 is I-1, valor_celda(Tablero, I2, J, Jugador). % misma columna, fila superior
ver_adyacentes(Tablero, I, J, Jugador, I3, J):- I3 is I+1, valor_celda(Tablero, I3, J, Jugador). % misma columna, fila inferior

%% PREDICADOS AUXILIARES - hay_movimiento
%% ----------------------------------------------------------------------------------------------------------------------------------------------

% hay_movimiento_celda(+Tablero, I, J) -> es exitoso si hay algún movimiento posible desde la celda (I,J)
hay_movimiento_celda(Tablero, I, J) :- J1 is J - 1, valor_celda(Tablero, I, J1, -). % misma fila, columna izquierda
hay_movimiento_celda(Tablero, I, J) :- J1 is J + 1, valor_celda(Tablero, I, J1, -). % misma fila, columna derecha
hay_movimiento_celda(Tablero, I, J) :- I1 is I - 1, valor_celda(Tablero, I1, J, -). % misma columna, fila superior
hay_movimiento_celda(Tablero, I, J) :- I1 is I + 1, valor_celda(Tablero, I1, J, -). % misma columna, fila inferior

%% PREDICADOS AUXILIARES - hay_posible_captura
%% ----------------------------------------------------------------------------------------------------------------------------------------------

% hay_posible_captura_celda(Tablero, I, J, Jugador) -> Dada una matriz revisa si la pieza en I,J es capturable por el jugador Jugador
hay_posible_captura_celda(Matriz, I, J, Jugador) :- % captura horizontal
    ver_adyacentes(Matriz, I, J, Jugador, F, _),
    ver_adyacentes(Matriz, I, J, -, F, C),
    ver_adyacentes(Matriz, F, C, Jugador, _, _),
    !.
hay_posible_captura_celda(Matriz, I, J, Jugador) :- % captura vertical
    ver_adyacentes(Matriz, I, J, Jugador, _, C),
    ver_adyacentes(Matriz, I, J, Jugador, F, C),
    ver_adyacentes(Matriz, F, C, Jugador, _, _),
    !.

%% PREDICADOS AUXILIARES - hacer_movimiento
%% ----------------------------------------------------------------------------------------------------------------------------------------------

% capturar_norte(+Tablero, +I, +J, +Jugador, +JugadorOpuesto, -CoordenadaCaptura) -> Comprueba si la pieza al norte de (I,J) es capturable
capturar_norte(Tablero, I, J, Jugador, JugadorOpuesto, I2, exito) :-
    I2 is I-1, I3 is I-2,
    valor_celda(Tablero, I2, J, JugadorOpuesto),
    valor_celda(Tablero, I3, J, Jugador).
% capturar_sur(+Tablero, +I, +J, +Jugador, +JugadorOpuesto, -CoordenadaCaptura) -> Comprueba si la pieza al sur de (I,J) es capturable
capturar_sur(Tablero, I, J, Jugador, JugadorOpuesto, I2, exito) :-
    I2 is I+1, I3 is I+2,
    valor_celda(Tablero, I2, J, JugadorOpuesto),
    valor_celda(Tablero, I3, J, Jugador).
% capturar_este(+Tablero, +I, +J, +Jugador, +JugadorOpuesto, -CoordenadaCaptura) -> Comprueba si la pieza al oeste de (I,J) es capturable
capturar_oeste(Tablero, I, J, Jugador, JugadorOpuesto, J2, exito) :-
    J2 is J-1, J3 is J-2,
    valor_celda(Tablero, I, J2, JugadorOpuesto),
    valor_celda(Tablero, I, J3, Jugador).
% capturar_este(+Tablero, +I, +J, +Jugador, +JugadorOpuesto, -CoordenadaCaptura) -> Comprueba si la pieza al este de (I,J) es capturable
capturar_este(Tablero, I, J, Jugador, JugadorOpuesto, J2, exito) :-
    J2 is J+1, J3 is J+2,
    valor_celda(Tablero, I, J2, JugadorOpuesto),
    valor_celda(Tablero, I, J3, Jugador).

%% PREDICADOS AUXILIARES - mejor_movimiento
%% ----------------------------------------------------------------------------------------------------------------------------------------------

%% Reviar los cuts
%   --------------------------------------------------

% contar_piezas(+Tablero, -PiezasX, -PiezasO) -> Dada una matriz tablero, devuelve la cantidad de piezas de cada jugador
contar_piezas(Tablero, PiezasX, PiezasO) :-
    findall((I,J), valor_celda(Tablero, I, J, x), ListaPiezasX), length(ListaPiezasX, PiezasX),
    findall((I,J), valor_celda(Tablero, I, J, o), ListaPiezasO), length(ListaPiezasO, PiezasO).

% chequear_final(+Estado, -Resultado) -> Comprueba que se cumplan las condiciones de finalización del juego
% Jugador X va 3 turnos sin moverse -> gana O
chequear_final(Estado, -99999) :- arg(2, Estado, 3), !.
% Jugador O va 3 turnos sin moverse -> gana X
chequear_final(Estado, 99999) :- arg(3, Estado, 3), !.
% Ambos jugadores van 12 o más jugadas sin capturar -> empate 
chequear_final(Estado, 0) :-
    arg(4, Estado, SinCapturarX), SinCapturarX >= 12,
    arg(5, Estado, SinCapturarO), SinCapturarO >= 12,
    !.
% Ya no quedan piezas de alguno de los 2 jugadores
chequear_final(Estado, Resultado) :-
    arg(1, Estado, Tablero),
    contar_piezas(Tablero, PiezasX, PiezasO),
    (
        % Gana O
        PiezasX == 0, Resultado = -99999
        ; 
        % Gana X
        PiezasO == 0, Resultado = 99999
    ),
    !.

% que deberia devolver -- chequear_final(estado(m(f(-,-,-,-,-),f(-,-,x,x,-),f(-,-,-,x,-),f(x,x,-,x,-),f(x,x,-,-,-)),5,0,0,0,0), R).

actualizar_sin_movimiento(Jugador, Estado, Estado2).

%
calcular_posibles_estados(Jugador, EstadoBase, Estados) :-
    findall(Estado2, hacer_movimiento_aux(EstadoBase, Jugador, _FO, _CO, _FD, _CD, normal, Estado2), Estados).


%% PREDICADOS INTERNOS
%% ----------------------------------------------------------------------------------------------------------------------------------------------

% hacer_movimiento_aux(+Estado, +FilaOrigen,+ColumnaOrigen,+FilaDestino,+ColumnaDestino,+TipoMovimiento,-Estado2).
hacer_movimiento_aux(Estado, Jugador, FilaOrigen, ColumnaOrigen, FilaDestino, ColumnaDestino, TipoMovimiento, Estado2) :-
    % Validación de movimiento
    arg(1, Estado, Tablero), % 1. Obtener tablero
    valor_celda(Tablero, FilaOrigen, ColumnaOrigen, Jugador), % 2. Obtener valor de casilla origen
    (Jugador = x ; Jugador = o), % 3. Chequear que la casilla origen no está vacía
    valor_celda(Tablero, FilaDestino, ColumnaDestino, -), % 4. Chequear que la casilla destino esta vacía
    ver_adyacentes(Tablero, FilaOrigen, ColumnaOrigen, -, FilaDestino, ColumnaDestino), % 5. Chequear que el movimiento es solo hacia casillas vacías adyacentes
    (TipoMovimiento = con_captura -> hay_posible_captura(Estado, Jugador) ; true), % 6. Chequear que haya captura posible si el movimiento es con_captura
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
