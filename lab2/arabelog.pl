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

%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% ---------------------------------------------------------------------- LÓGICA DEL JUEGO ----------------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%% PREDICADOS AUXILIARES - Tablero
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% es_centro(?I, ?J) -> Dado un par de coordenadas I,J determina si son el centro del tablero o no
es_centro(3, 3).

% jugador_opuesto(?Jugador, ?JugadorOpuesto) -> Dado un tipo de Jugador, da el otro tipo de jugador en JugadorOpuesto
jugador_opuesto(o, x).
jugador_opuesto(x, o).

% valor_celda(+Tablero, ?I, ?J, ?Valor) -> Dada una matriz functor y coordenadas I y J, devuelve el valor de dicha celda
valor_celda(Tablero, I, J, Valor) :-
    arg(I, Tablero, Fila),
    arg(J, Fila, Valor).

% modificar_celda(+Tablero, +I, +J, +Valor) -> Dada una matriz functor, coordenadas I y J, y un Valor, devuelve la matriz resultante de sustituir el valor en la celda (I,J) 
% con el valor Valor (modifica la matriz original)
modificar_celda(Tablero, I, J, Valor) :-
    arg(I, Tablero, Fila),
    setarg(J, Fila, Valor),
    setarg(I, Tablero, Fila).

% ver_adyacentes(Tablero, I, J, Jugador, M, N) -> Dada una matriz revisa si la pieza en coordenadas I,J es adyacente a una pieza del jugador Jugador y da sus coordenadas en M,N.
ver_adyacentes(Tablero, I, J, Jugador, I, J2):- J2 is J-1, valor_celda(Tablero, I, J2, Jugador). % misma fila, columna izquierda
ver_adyacentes(Tablero, I, J, Jugador, I, J3):- J3 is J+1, valor_celda(Tablero, I, J3, Jugador). % misma fila, columna derecha
ver_adyacentes(Tablero, I, J, Jugador, I2, J):- I2 is I-1, valor_celda(Tablero, I2, J, Jugador). % misma columna, fila superior
ver_adyacentes(Tablero, I, J, Jugador, I3, J):- I3 is I+1, valor_celda(Tablero, I3, J, Jugador). % misma columna, fila inferior

% contar_piezas(+Tablero, -PiezasX, -PiezasO) -> Dada una matriz tablero, devuelve la cantidad de piezas de cada jugador
contar_piezas(Tablero, PiezasX, PiezasO) :-
    findall((I,J), valor_celda(Tablero, I, J, x), ListaPiezasX), length(ListaPiezasX, PiezasX),
    findall((I,J), valor_celda(Tablero, I, J, o), ListaPiezasO), length(ListaPiezasO, PiezasO).

%% PREDICADOS AUXILIARES - Chequeos
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% hay_movimiento_celda(+Tablero, I, J) -> es exitoso si hay algún movimiento posible desde la celda (I,J)
hay_movimiento_celda(Tablero, I, J) :- J1 is J - 1, valor_celda(Tablero, I, J1, -). % misma fila, columna izquierda
hay_movimiento_celda(Tablero, I, J) :- J1 is J + 1, valor_celda(Tablero, I, J1, -). % misma fila, columna derecha
hay_movimiento_celda(Tablero, I, J) :- I1 is I - 1, valor_celda(Tablero, I1, J, -). % misma columna, fila superior
hay_movimiento_celda(Tablero, I, J) :- I1 is I + 1, valor_celda(Tablero, I1, J, -). % misma columna, fila inferior

% hay_posible_captura_celda(Tablero, I, J, Jugador) -> Dada una matriz revisa si la pieza en I,J es capturable por el jugador Jugador
hay_posible_captura_celda(Matriz, I, J, Jugador) :- % captura horizontal
    ver_adyacentes(Matriz, I, J, Jugador, F, _),
    ver_adyacentes(Matriz, I, J, -, F, C),
    ver_adyacentes(Matriz, F, C, Jugador, _, _),
    !.
hay_posible_captura_celda(Matriz, I, J, Jugador) :- % captura vertical
    ver_adyacentes(Matriz, I, J, Jugador, _, C),
    ver_adyacentes(Matriz, I, J, -, F, C),
    ver_adyacentes(Matriz, F, C, Jugador, _, _),
    !.

%% PREDICADOS AUXILIARES - Capturas
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% capturar_norte(+Tablero, +I, +J, +Jugador, +JugadorOpuesto, -CoordenadaCaptura) -> Comprueba si la pieza al norte de (I,J) es capturable
capturar_norte(Tablero, I, J, Jugador, JugadorOpuesto, I2, exito) :-
    I2 is I-1, I3 is I-2,
    \+es_centro(I2, J),
    valor_celda(Tablero, I2, J, JugadorOpuesto),
    valor_celda(Tablero, I3, J, Jugador).
% capturar_sur(+Tablero, +I, +J, +Jugador, +JugadorOpuesto, -CoordenadaCaptura) -> Comprueba si la pieza al sur de (I,J) es capturable
capturar_sur(Tablero, I, J, Jugador, JugadorOpuesto, I2, exito) :-
    I2 is I+1, I3 is I+2,
    \+es_centro(I2, J),
    valor_celda(Tablero, I2, J, JugadorOpuesto),
    valor_celda(Tablero, I3, J, Jugador).
% capturar_este(+Tablero, +I, +J, +Jugador, +JugadorOpuesto, -CoordenadaCaptura) -> Comprueba si la pieza al oeste de (I,J) es capturable
capturar_oeste(Tablero, I, J, Jugador, JugadorOpuesto, J2, exito) :-
    J2 is J-1, J3 is J-2,
    \+es_centro(I, J2),
    valor_celda(Tablero, I, J2, JugadorOpuesto),
    valor_celda(Tablero, I, J3, Jugador).
% capturar_este(+Tablero, +I, +J, +Jugador, +JugadorOpuesto, -CoordenadaCaptura) -> Comprueba si la pieza al este de (I,J) es capturable
capturar_este(Tablero, I, J, Jugador, JugadorOpuesto, J2, exito) :-
    J2 is J+1, J3 is J+2,
    \+es_centro(I, J2),
    valor_celda(Tablero, I, J2, JugadorOpuesto),
    valor_celda(Tablero, I, J3, Jugador).

%% PREDICADOS AUXILIARES - Movimientos
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% calcular_posibles_estados(+Jugador, +EstadoBase, -Estados) -> Dado un jugador y un estado base, genera una lista con todos los estados posibles resultantes de que el jugador
% haga un movimiento normal. A su vez, si en dicho movimiento realiza una captura, sigue intentando capturar hasta que no le queden más posibles capturas.
calcular_posibles_estados(Jugador, EstadoBase, Estados) :-
    findall(Estado, hacer_movimiento_aux(EstadoBase, Jugador, _FO, _CO, _FD, _CD, normal, Estado, si), EstadosConCaptura),
    findall(Estado, hacer_movimiento_aux(EstadoBase, Jugador, _FO1, _CO1, _FD1, _CD1, normal, Estado, no), EstadosSinCaptura),
    calcular_posibles_estados_captura(Jugador, EstadosConCaptura, EstadosConCaptura2),
    append(EstadosConCaptura2, EstadosSinCaptura, Estados).

% calcular_posibles_estados_captura(+Jugador, +Estados, -Estados2) -> En base a una lista de estados donde se realizó una captura, sigue intentando hacer movimientos con captura
% hasta que no queden más movimientos posibles para cada estado de la lista. De esta forma, la lista salida tiene los estados de la lista entrada pero en una etapa más avanzada
% (de ser posible, en caso contrario, es el mismo estado).
% Paso Base -> No quedan estados
calcular_posibles_estados_captura(_Jugador, [], []).
% Paso Base -> Hay un estado solo
calcular_posibles_estados_captura(Jugador, [Estado], [EstadoFinal]) :-
    calcular_estado_final(Jugador, Estado, EstadoFinal), !.
% Paso Inductivo -> Quedan varios estados
calcular_posibles_estados_captura(Jugador, [Estado | Estados], [EstadoFinal | EstadosFinales]) :-
    calcular_posibles_estados_captura(Jugador, Estados, EstadosFinales),
    calcular_estado_final(Jugador, Estado, EstadoFinal).

% calcular_estado_final(+Jugador, +Estado, -EstadoFinal) -> Intenta realizar todas las capturas posibles en Estado con el jugador Jugador. El estado resultante es EstadoFinal
% Caso son capturas:
calcular_estado_final(Jugador, Estado, EstadoFinal) :-
    hacer_movimiento_aux(Estado, Jugador, _FO, _CO, _FD, _CD, con_captura, Estado2, _Captura),
    calcular_estado_final(Jugador, Estado2, EstadoFinal), !. % Sin el cut unifica con las jugadas intermedias
% Caso sin capturas:
calcular_estado_final(_Jugador, Estado, Estado).

% hacer_movimiento_aux(+Estado, +FilaOrigen,+ColumnaOrigen,+FilaDestino,+ColumnaDestino,+TipoMovimiento,-Estado2) -> Igual a hacer_movimiento pero tiene un parámetro extra que
% funciona como flag para determinar si se capturó o no. Esto es necesario para determinar los estados con captura en calcular_posibles_estados
hacer_movimiento_aux(Estado, Jugador, FilaOrigen, ColumnaOrigen, FilaDestino, ColumnaDestino, TipoMovimiento, Estado2, Captura) :-
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
    (Exito == exito -> Captura = si ; Captura = no),
    (TipoMovimiento = con_captura -> Exito == exito ; true).

%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% ----------------------------------------------------------------------- LÓGICA DE LA IA ----------------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%% PREDICADOS FASE 1
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% distancia_al_centro(+I, +J, -Distancia) -> Dado un par de coordenadas (I,J) calcula la distancia manhattan hacia (3,3)
distancia_al_centro(I, J, Distancia) :- Distancia is abs(I - 3) + abs(J - 3).

% mas_proximo_al_centro_2(+ListaCeldasLibres, +I_actual, +J_actual, MinActual, -I_final, -J_final ) -> Intenta elegir celdas cercanas al centro
% Sin celdas libres
mas_proximo_al_centro_2([], I, J, _MinActual, I, J).
% Hay celdas libres alrededor del centro
mas_proximo_al_centro_2([(I,J) | _L], _I_actual, _J_actual, _MinActual, I, J) :- 
    distancia_al_centro(I, J, Distancia),
    Distancia == 1,
    !.
% Todavía hay celdas cercanas al centro
mas_proximo_al_centro_2([(I,J) | L], _I_actual, _J_actual, MinActual, I_final, J_final) :- 
    distancia_al_centro(I, J, Distancia),
    Distancia < MinActual,
    !,
    mas_proximo_al_centro_2(L, I, J, Distancia, I_final, J_final).
% Buscando celdas que cumplan las anteriores condiciones
mas_proximo_al_centro_2([(_I, _J) | L], I_actual, J_actual, MinActual, I_final, J_final) :-
    mas_proximo_al_centro_2(L, I_actual, J_actual, MinActual, I_final, J_final).

% mas_proximo_al_centro(+ListaCeldasLibres, -I_final, -J_final) ->
% Caso borde, en teoria no deberia invocarse este predicado con lista vacia
mas_proximo_al_centro([], 99, 99).
% Caso genérico, calcula la distancia al centro de la primer celda libre y comienza a calcular
mas_proximo_al_centro([(I,J) | L], I_final, J_final) :-
    distancia_al_centro(I, J, Distancia),
    mas_proximo_al_centro_2(L, I, J, Distancia, I_final, J_final ).

% hacer_movimiento_fase1(+Tablero, +Jugador, +Estrategia)
% VERSION DUMMY
hacer_movimiento_fase1(Tablero, Jugador, dummy) :-
    % Elegir ficha 1
    valor_celda(Tablero, I, J, -),
    not(es_centro(I, J)),
    modificar_celda(Tablero, I, J, Jugador),
    % Elegir ficha 2
    valor_celda(Tablero, I2, J2, -),
    not(es_centro(I2, J2)),
    modificar_celda(Tablero, I2, J2, Jugador),
    !.
% VERSION HEURÍSTICA 
% caso borde - minimo tiene que estar el centro y dos casillas más libres, en caso contrario devuelve el mismo tablero
hacer_movimiento_fase1(Tablero, _Jugador, minimax) :-
    findall((I,J), valor_celda(Tablero, I, J, -), ListaCeldasLibres),
    length(ListaCeldasLibres, L),
    L < 3, 
    !.
%
hacer_movimiento_fase1(Tablero, Jugador, minimax) :-
    findall((I,J), valor_celda(Tablero, I, J, -), ListaCeldasLibres),
    % Descarto el centro
    select((3,3), ListaCeldasLibres, ListaCeldasLibres2),
    % Elegir ficha 1
    mas_proximo_al_centro(ListaCeldasLibres2, I, J ),
    modificar_celda(Tablero, I, J, Jugador),
    select((I, J), ListaCeldasLibres2, ListaCeldasLibres3),
    % Elegir ficha 2
    mas_proximo_al_centro(ListaCeldasLibres3, I2, J2 ),
    modificar_celda(Tablero, I2, J2, Jugador),
    !.

%% PREDICADOS FASE 2 - PRINCIPALES MINIMAX
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% minimax(+Nivel, +Alpha, +Beta, +Jugador, +EstadoBase, -EstadoFinal, -Puntaje) -> Ejecución recursiva de minimax que retorna la jugada con mejor puntaje dentro del árbol
% Paso Base -> Nivel = 0
minimax(0, _Alpha, _Beta, _Jugador, EstadoFinal, EstadoFinal, Puntaje) :-
    calcular_puntaje_minimax_hoja(EstadoFinal, Puntaje), !.
% Paso Base -> Nivel > 0, Fin de juego
minimax(_Nivel, _Alpha, _Beta, _Jugador, EstadoFinal, EstadoFinal, Puntaje) :-
    chequear_final(EstadoFinal, Puntaje), !.
% Paso Base -> Nivel > 0, No hay posibles movimientos
minimax(_Nivel, _Alpha, _Beta, Jugador, EstadoBase, EstadoBase, Puntaje) :-
    calcular_posibles_estados(Jugador, EstadoBase, []),
    calcular_puntaje_minimax_hoja(EstadoBase, Puntaje), !.
% Paso Inductivo -> Hay posibles movimientos
minimax(Nivel, Alpha, Beta, Jugador, EstadoBase, EstadoFinal, Puntaje) :-
    calcular_posibles_estados(Jugador, EstadoBase, [Estado | Estados]), % Aca se hacen todos los movimientos posibles
    calcular_puntaje_minimax_rama(Nivel, Alpha, Beta, Estado, Jugador, [Estado | Estados], EstadoFinal, Puntaje).  

% calcular_puntaje_minimax_hoja(+Estado, -Puntaje) -> Calcula el puntaje del estado actual tomando como heurística la diferencia entre cantidad de piezas de cada jugador
calcular_puntaje_minimax_hoja(Estado, Puntaje) :-
    arg(1, Estado, Tablero), % 1. Obtener tablero del estado
    contar_piezas(Tablero, PiezasX, PiezasO), % 2. Contar cantidad de piezas de cada jugador
    Puntaje is PiezasX - PiezasO. % 3. Calcular heurística como diferencia entre piezas de X y O

% calcular_puntaje_minimax_rama(+Nivel, +Alpha, +Beta, +MejorEstado, +Jugador, +Estados, -EstadoFinal, -Puntaje) -> Calcula el puntaje de la rama actual y guarda el mejor estado
% en caso de tener que sobreescribirlo
% Paso Base -> 
calcular_puntaje_minimax_rama(Nivel, Alpha, Beta, MejorEstado, Jugador, [Estado],  EstadoFinal, Puntaje) :-
    Nivel2 is Nivel - 1,
    jugador_opuesto(Jugador, Contrincante),
    minimax(Nivel2, Alpha, Beta, Contrincante, Estado, _EstadoFinal2, Puntaje2), 
    estrategia_jugador(Jugador, Estrategia),
    calcular_mejor_jugada(Alpha, Beta, Estrategia, Puntaje2, MejorEstado, Estado, Puntaje, EstadoFinal).
% Paso Inductivo ->
calcular_puntaje_minimax_rama(Nivel, Alpha, Beta, MejorEstado, Jugador, [Estado | Estados], EstadoFinal, Puntaje) :-
    Estados \= [],
    Nivel2 is Nivel - 1,
    jugador_opuesto(Jugador, Contrincante),
    minimax(Nivel2, Alpha, Beta, Contrincante, Estado, _EstadoFinal2, Puntaje2),
    estrategia_jugador(Jugador, Estrategia),
    calcular_alpha_beta(Alpha, Beta, Estrategia, Puntaje2, Alpha2, Beta2),
    (
        Alpha2 >= Beta2 
        ->
            EstadoFinal = Estado,
            Puntaje = Puntaje2,
            !
        ;
            calcular_puntaje_minimax_rama(Nivel, Alpha2, Beta2, MejorEstado, Jugador, Estados, EstadoFinal, Puntaje)
    ).

%% PREDICADOS AUXILIARES MINIMAX
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% pre_minimax(?Alpha, ?Beta) -> Determina los valores iniciales de alpha y beta antes de iniciar la ejecución de minimax
pre_minimax(-99999, 99999).

% estrategia_jugador(?Jugador, ?Estrategia) -> Determina la estrategia minimax según el tipo de jugador
estrategia_jugador(x, maximizar).
estrategia_jugador(o, minimizar).

% calcular_alpha_beta(+Alpha, +Beta, +Estrategia, +Puntaje, -Alpha2, -Beta2) -> Actualiza los valores de alpha y beta basados en la estrategia y puntaje obtenido en la última ejecución
calcular_alpha_beta(Alpha, Beta, maximizar, Puntaje, Alpha2, Beta) :-
    Alpha2 is max(Alpha, Puntaje), !.
calcular_alpha_beta(Alpha, Beta, minimizar, Puntaje, Alpha, Beta2) :-
    Beta2 is min(Beta, Puntaje), !.

% calcular_mejor_jugada(+Alpha, +Beta, +Estrategia, +Puntaje, +EstadoAnterior, +EstadoActual, -MejorPuntaje, -MejorEstado) -> Elige el mejor puntaje y estado en base a los 
% alpha y beta y la jugada anterior y actual
calcular_mejor_jugada(Alpha, _Beta, maximizar, Puntaje, _EstadoAnterior, EstadoActual, Puntaje, EstadoActual) :- Puntaje > Alpha.
calcular_mejor_jugada(Alpha, _Beta, maximizar, Puntaje, EstadoAnterior, _EstadoActual, Alpha, EstadoAnterior) :- Puntaje =< Alpha.
calcular_mejor_jugada(_Alpha, Beta, minimizar, Puntaje, _EstadoAnterior, EstadoActual, Puntaje, EstadoActual) :- Puntaje < Beta.
calcular_mejor_jugada(_Alpha, Beta, minimizar, Puntaje, EstadoAnterior, _EstadoActual, Beta, EstadoAnterior) :- Puntaje >= Beta.

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

%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% --------------------------------------------------------------------- PREDICADOS FINALES ---------------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% hay_movimiento(+Estado,+Jugador) -> es exitoso si hay algún movimiento posible para el jugador
hay_movimiento(Estado, Jugador) :-
    arg(1, Estado, Tablero),
    valor_celda(Tablero, I, J, Jugador),
    hay_movimiento_celda(Tablero, I, J),
    !.

% hay_posible_captura(+Estado, +Jugador,) -> dado un Estado y un jugador, comprueba si alguno de los movimientos que puede realizar lleva a una captura
hay_posible_captura(Estado, Jugador):-
    arg(1, Estado, Tablero),
    jugador_opuesto(Jugador, JugadorOpuesto),
    valor_celda(Tablero, I, J, JugadorOpuesto),
    \+es_centro(I, J),
    hay_posible_captura_celda(Tablero, I, J, Jugador),
    !.

% hacer_movimiento(+Estado, +FilaOrigen, +ColumnaOrigen +FilaDestino, +ColumnaDestino, +TipoMovimiento, -Estado2) ->
% Hace el movimiento a partir del Estado inicial del tablero, las coordenadas origen y destino y el tipo de movimiento.
% El tipo de movimiento puede ser normal o con_captura. En el segundo caso, el movimiento debe incluir una captura, o fallar.
% Estado y Estado2 tienen el mismo término, solamente se utiliza para poder consultar la variable de salida desde el bridge
hacer_movimiento(Estado, FilaOrigen, ColumnaOrigen, FilaDestino, ColumnaDestino, TipoMovimiento, Estado2) :-
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

% mejor_movimiento(+Estado, +Jugador, +NivelMinimax, +Estrategia, -Estado2) -> dado un estado, un jugador, un nivel para minimax, y una estrategia, devuelve la mejor jugada posible. 
% Estrategia es solamente un átomo que se le asigna para poder implementar más de una estrategia.
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
% -> Fase 1: Rellena según heurística: cuánto más cerca del centro, mejor.
% -> Fase 2: Elige su movimiento ejecutando minimax
mejor_movimiento(Estado, Jugador, Nivel, minimax, Estado2) :-
    copy_term(Estado, EstadoAux), % 1. Copiar estado para no editar tablero original
    arg(1, EstadoAux, Tablero), % 2. Obtener tablero del estado
    (arg(6, EstadoAux, 1) % 3. Chequear fase
        -> % Si fase 1
            hacer_movimiento_fase1(Tablero, Jugador, minimax), % 3.1.1. Elige según heurística
            EstadoAux = Estado2 % 3.1.2. El tablero modificado es el de EstadoAux, así que unifica con Estado2
        ; % Si fase 2
            pre_minimax(Alpha, Beta), % 3.2.1. Inicializa los valores de Alpha y Beta
            minimax(Nivel, Alpha, Beta, Jugador, EstadoAux, Estado2, _) % Ejecuta minimax
    ).
