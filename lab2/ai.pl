:- module(ai,
[
    % GENERALES
    hacer_movimiento_fase1/3,
    % MINIMAX
    pre_minimax/2,
    minimax/7
]).

:- use_module(core).

%% PREDICADOS GENERALES
%% ----------------------------------------------------------------------------------------------------------------------------------------------

% hacer_movimiento(+Tablero, +Jugador, +Estrategia) -> VERSION DUMMY, FASE 1
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

%% PREDICADOS PRINCIPALES MINIMAX
%% ----------------------------------------------------------------------------------------------------------------------------------------------

% minimax(+Nivel, +Alpha, +Beta, +Jugador, +EstadoBase, -EstadoFinal, -Puntaje) ->
% Paso Base -> Nivel = 0
minimax(0, _Alpha, _Beta, Jugador, EstadoFinal, EstadoFinal, Puntaje) :-
    calcular_puntaje_minimax_hoja(Jugador, EstadoFinal, Puntaje), !.
% Paso Base -> Nivel > 0, Fin de juego
minimax(_Nivel, _Alpha, _Beta, _Jugador, EstadoFinal, EstadoFinal, Puntaje) :-
    chequear_final(EstadoFinal, Puntaje), !.
% Paso Base -> Nivel > 0, No hay posibles movimientos
minimax(Nivel, Alpha, Beta, Jugador, EstadoBase, EstadoBase, Puntaje) :-
    calcular_posibles_estados(Jugador, EstadoBase, []),
    calcular_puntaje_minimax_hoja(Jugador, EstadoBase, Puntaje), !.
% Paso Inductivo -> Hay posibles movimientos
minimax(Nivel, Alpha, Beta, Jugador, EstadoBase, EstadoFinal, Puntaje) :-
    calcular_posibles_estados(Jugador, EstadoBase, [Estado | Estados]), % Aca se hacen todos los movimientos posibles
    calcular_puntaje_minimax_rama(Nivel, Alpha, Beta, Estado, Jugador, [Estado | Estados], EstadoFinal, Puntaje).

% calcular_puntaje_minimax_hoja(+Jugador, +Estado, -Puntaje) -> 
calcular_puntaje_minimax_hoja(Jugador, Estado, Puntaje) :-
    arg(1, Estado, Tablero), % 1. Obtener tablero del estado
    contar_piezas(Tablero, PiezasX, PiezasO), % 2. Contar cantidad de piezas de cada jugador
    Puntaje is PiezasX - PiezasO. % 3. Calcular heurística como diferencia entre piezas de X y O

% calcular_puntaje_minimax_rama(+Nivel, +Alpha, +Beta, +Jugador, +EstadoBase, +Estados, -EstadoFinal, -Puntaje) ->
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
            EstadoFinal = Estado, % REVISAR
            Puntaje = Puntaje2,
            !
        ;
            calcular_puntaje_minimax_rama(Nivel, Alpha2, Beta2, MejorEstado, Jugador, Estados, EstadoFinal, Puntaje)
    ).

%% PREDICADOS AUXILIARES MINIMAX
%% ----------------------------------------------------------------------------------------------------------------------------------------------

% pre_minimax(?Alpha, ?Beta) -> Determina los valores iniciales de alpha y beta antes de iniciar la ejecución de minimax
pre_minimax(-99999, 99999).

% estrategia_jugador(?Jugador, ?Estrategia) -> Determina la estrategia minimax según el tipo de jugador
estrategia_jugador(x, maximizar).
estrategia_jugador(o, minimizar).

% calcular_alpha_beta(+Alpha, +Beta, +Estrategia, +Puntaje, -Alpha2, -Beta2) -> Actualiza los valores de alpha y beta basados en la estrategia
% y puntaje obtenido en la última ejecución de minimax
calcular_alpha_beta(Alpha, Beta, maximizar, Puntaje, Alpha2, Beta) :-
    Alpha2 is max(Alpha, Puntaje), !.
calcular_alpha_beta(Alpha, Beta, minimizar, Puntaje, Alpha, Beta2) :-
    Beta2 is min(Beta, Puntaje), !.

% calcular_mejor_jugada(+Alpha, +Beta, +Estrategia, +Puntaje, +EstadoAnterior, +EstadoActual, -MejorPuntaje, -MejorEstado) -> Elige
% el mejor puntaje y estado en base a los alpha y beta y la jugada anterior y actual
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
% que deberia devolver -- chequear_final(estado(m(f(-,-,-,-,-),f(-,-,x,x,-),f(-,-,-,x,-),f(x,x,-,x,-),f(x,x,-,-,-)),5,0,0,0,0), R).
