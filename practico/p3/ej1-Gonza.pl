%Se   busca   colorear   un   mapa,   de   forma   que   no   haya   dos países vecinos con los mismos colores.
%El   mapa   se   representa   por   una   lista   de   regiones   de   la forma:

% Definicion del Mapa
% ----------------------------------------------------------------------------------------------------------

% region(Nombre,  Color,  ColoresDeVecinos)
region(_, _, _).

% Predicados Auxiliares
% ----------------------------------------------------------------------------------------------------------
pintarMapa([],_).
pintarMapa([region(_, Color, _) | LR], Colores) :- 
    member(Color, Colores),
    pintarMapa(LR, Colores).

revisarAdyacencia([]).
revisarAdyacencia([ region(_,  Color,  ColoresDeVecinos) | LR]) :- 
    not(member(Color, ColoresDeVecinos)),
    revisarAdyacencia(LR).


% Predicado Principal
% ----------------------------------------------------------------------------------------------------------


% colorear(Mapa, Colores) <- Mapa (lista de regiones) se encuentra coloreado con Colores, de forma tal que no hay dos vecinos iguales.
colorear([HR | LR], Colores) :-
    pintarMapa([HR | LR], Colores),
    revisarAdyacencia([HR | LR]).
