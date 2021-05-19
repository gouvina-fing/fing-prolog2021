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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREDICADOS AUXILIARES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% valor_celda(+Matriz, +I, +J, -Valor) -> Dada una matriz functor y coordenadas I y J, devuelve el valor de dicha celda
valor_celda(Matriz, I, J, Valor) :-
    arg(I, Matriz, Fila),
    arg(J, Fila, Valor).

% modificar_celda(Matriz, I, J, Valor) -> Dada una matriz functor, coordenadas I y J, y un Valor, devuelve la matriz resultante
% de sustituir el valor en la celda (I,J) con el valor Valor (modifica la matriz original)
modificar_celda(Matriz, I, J, Valor) :-
    arg(I, Matriz, Fila),
    setarg(J, Fila, Valor),
    setarg(I, Matriz, Fila).

% chequear_captura(Matriz, I, J) -> Dada una matriz functor y coordenadas I y J, chequea si la pieza en (I,J) es capturada
% tanto horizontal como verticalmente, sin importar el jugador.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ESTO ES INEFICIENTE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chequear_captura(Matriz, I, J) :-
    valor_celda(Matriz, I, J, x),
    valor_celda(Matriz, I, J2, o), J2 is J-1,
    valor_celda(Matriz, I, J3, o), J3 is J+1.
chequear_captura(Matriz, I, J) :-
    valor_celda(Matriz, I, J, x),
    valor_celda(Matriz, I2, J, o), I2 is I-1,
    valor_celda(Matriz, I3, J, o), I3 is I+1.
chequear_captura(Matriz, I, J) :-
    valor_celda(Matriz, I, J, o),
    valor_celda(Matriz, I, J2, x), J2 is J-1,
    valor_celda(Matriz, I, J3, x), J3 is J+1.
chequear_captura(Matriz, I, J) :-
    valor_celda(Matriz, I, J, o),
    valor_celda(Matriz, I2, J, x), I2 is I-1,
    valor_celda(Matriz, I3, J, x), I3 is I+1.

% ver_adyacentes(Matriz, I, J, Tipo, M,N): Dada una matriz revisa si la pieza en coordenadas I,J es adyacente a una pieza
% del tipo tipo y da sus coordenadas en M,N.
ver_adyacentes(Matriz, I, J, Tipo, I, J2):-
    valor_celda(Matriz, I, J2, Tipo), J2 is J-1.
ver_adyacentes(Matriz, I, J, Tipo, I, J3):-
    valor_celda(Matriz, I, J3, Tipo), J3 is J+1.
ver_adyacentes(Matriz, I, J, Tipo, I2, J):-
    valor_celda(Matriz, I2, J, Tipo), I2 is I-1.
ver_adyacentes(Matriz, I, J, Tipo, I3, J):-
    valor_celda(Matriz, I3, J, Tipo), I3 is I+1.

% capturable(Matriz, I, J, Tipo):Dada una matriz revisa si la pieza en I,J es capturable por el tipo de pieza Tipo
capturable(Matriz,I,J, x):-
    valor_celda(Matriz, I, J, o),
    ver_adyacentes(Matriz, I, J, x,M,_),
    ver_adyacentes(Matriz, I, J, -,M,R),
    ver_adyacentes(Matriz, M, R , x,_,_).
capturable(Matriz,I,J, x):-
    valor_celda(Matriz, I, J, o),
    ver_adyacentes(Matriz, I, J, x,_,N),
    ver_adyacentes(Matriz, I, J, -,R,N),
    ver_adyacentes(Matriz, R, N, x,_,_).
capturable(Matriz,I,J, x):-
    valor_celda(Matriz, I, J, x),
    ver_adyacentes(Matriz, I, J, o,M,_),
    ver_adyacentes(Matriz, I, J, -,M,R),
    ver_adyacentes(Matriz, M, R, o ,_,_).
capturable(Matriz,I,J, x):-
    valor_celda(Matriz, I, J, x),
    ver_adyacentes(Matriz, I, J, o,_,N),
    ver_adyacentes(Matriz, I, J, -,R,N),
    ver_adyacentes(Matriz, R, N, o,_,_).
=======
% hay_movimiento_celda(+Tablero, I, J) -> es exitoso si hay algún movimiento posible desde la celda (I,J)
hay_movimiento_celda(Tablero, I, J) :- J < 5, J1 is J + 1, valor_celda(Tablero, I, J1, -), !. % misma fila, columna derecha
hay_movimiento_celda(Tablero, I, J) :- J > 1, J1 is J - 1, valor_celda(Tablero, I, J1, -), !. % misma fila, columna izquierda
hay_movimiento_celda(Tablero, I, J) :- I < 5, I1 is I + 1, valor_celda(Tablero, I1, J, -), !. % misma columna, fila inferior
hay_movimiento_celda(Tablero, I, J) :- I > 1, I1 is I - 1, valor_celda(Tablero, I1, J, -), !. % misma columna, fila superior


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREDICADOS PRINCIPALES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% hay_movimiento(+Estado,+Jugador): es exitoso si hay algún movimiento posible para el jugador
% dos caminos:
% - agarro todas mis piezas y busco si alguna tiene una casilla vacia a la que ir
% - agarro todas las casillas vacias y veo si al rededor de ella hay una pieza mia
hay_movimiento(Estado, Jugador) :-
    arg(1, Estado, Tablero),
    valor_celda(Tablero, I,J, Jugador),
    hay_movimiento_celda(Tablero, I, J),
    !.

% hay_posible_captura(+Estado, +Jugador): dado un Estado y un jugador, veo si alguno de los movimientos que puede realizar lleva a una captura
% hay_posible_captura(Estado, Jugador).
hay_posible_captura((Tablero, _, _, _, _, 2), Jugador):-
   capturable(Tablero,I,J, Jugador),
   !.


% hacer_movimiento((m(f(x,o,x,o,x),f(o,x,o,x,o),f(x,o,-,o,x),f(o,x,o,x,o),f(x,o,x,o,x)),0,0,0,0,2),3,2,3,3,normal,e).
% Hace el movimiento a partir del Estado inicial del tablero, las coordenadas origen y destino y el tipo de movimiento
% el tipo de movimiento puede ser normal o con_captura. En el segundo caso, el movimiento debe incluir una captura, o fallar.
% Estado y Estado2 tienen el mismo término, solamente se utiliza para poder consultar la variable de salida desde el bridge
% hacer_movimiento(+Estado, +FilaOrigen,+ColumnaOrigen,+FilaDestino,+ColumnaDestino,+TipoMovimiento,-Estado2).
hacer_movimiento(Estado, FilaOrigen, ColumnaOrigen, FilaDestino, ColumnaDestino, TipoMovimiento, Estado2) :-
    % Validación de movimiento
    arg(1, Estado, Tablero), % 1. Obtener tablero
    valor_celda(Tablero, FilaOrigen, ColumnaOrigen, Origen), % 2. Obtener valor de casilla origen
    (Origen = x ; Origen = o), % 3. Chequear que la casilla origen no está vacía
    valor_celda(Tablero, FilaDestino, ColumnaDestino, -), % 4. Chequear que la casilla destino esta vacía
    % Realización de movimiento
    copy_term(Estado, Estado2), % 1. Copiar estado2 para no sobreescribirlo
    arg(1, Estado, Tablero), % 2. Obtener tablero de estado2
    modificar_celda(Tablero, FilaOrigen, ColumnaOrigen, -), % 3. Vaciar celda origen
    modificar_celda(Tablero, FilaDestino, ColumnaDestino, Origen). % 4. Sobreescribir celda destino
    % Realización de captura
    %ColumnaDestino2 is ColumnaDestino + 1,
    %(chequear_captura(Tablero, FilaDestino, ColumnaDestino2) -> modificar_celda(Tablero, FilaDestino, ColumnaDestino2, -)).
	
% mejor_movimiento(+Estado,+Jugador,+NivelMinimax,+Estrategia,-Estado2): dado un estado, un jugador, un nivel para minimax, y una estrategia, 
% devuelve la mejor jugada posible. Estrategia es solamente un átomo que se le asigna para poder implementar más de una estrategia
% mejor_movimiento(Estado, Jugador, Nivel, Estrategia, Estado2)
