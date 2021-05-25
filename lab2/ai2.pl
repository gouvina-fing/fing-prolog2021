%https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwin97qvvNvwAhXPEbkGHT2sBJ8QFjAHegQIChAD&url=https%3A%2F%2Fwww.it.uu.se%2Fedu%2Fcourse%2Fhomepage%2Flogpro%2Fht09%2FAlphabeta%2Bothello.ppt&usg=AOvVaw01hRxVvWu2tiuhnvukWNyw

% move(Tablero, M)
move(_, _).

% mover(Tablero, Movida, Tablero2).
mover(_, _, _).

% evaluar(Tablero, Valor)
evaluar(_, _).
 
obtenerRival(x, o).
obtenerRival(o, x).


minimax(Tablero, Depth, Movida) :- minimax6(Depth, Tablero, x, max, _, Movida).

/* minimax6(+Depth, +Tablero, +Player, +Estrategia, -BestValue, -BestMove) :-
      BestMove es la mejor jugada que Player puede hacer en Tablero
      using the minimax algorithm searching Depth ply ahead.
*/
minimax6(0, Tablero, _, _, Valor, _) :- 
    evaluar(Tablero, Valor).
   % Value is V*Player.   % Value is from the current player’s perspective.

minimax6(D, Tablero, Player, Estrategia, Valor, Move) :-
    D > 0, 
    D1 is D - 1,
    %    hay_movimiento(Tablero, Player)     ---- en realidad recibe Estado
    % There must be at least one move!
    findall(M, move(Tablero, M), Moves),
    minimax9(Moves, Tablero, D1, Player, Estrategia, -1000, nil, Valor, Move).


/* minimax9(+Moves,+Tablero,+Depth,+Player,+Value0,+Move0,-BestValue,-BestMove)
    Moves es la lista de movimientos posibles que Player puede hacer en Tablero
    Value0 y Move0 son acumuladores de los mejores movimientos actuales
    BestValue y BestMove son los resultados finales
    
    */
minimax9([], _, _, _, _, Value, Best, Value, Best).

minimax9([Move|Moves],Tablero,D,Player1, maxim, Value0,Move0,BestValue,BestMove):-
    mover(Move, Tablero, Tablero2), 
    obtenerRival(Player1, Player2),

    % minimax6(D, Tablero2, Player2, minim, OppValue, OppMove), 
    minimax6(D, Tablero2, Player2, minim, OppValue, _), 
    
    
    ( OppValue > Value0 ->        
      minimax9(Moves,Tablero,D,Player1, maxim, OppValue ,Move ,BestValue,BestMove)
    ; minimax9(Moves,Tablero,D,Player1, maxim, Value0,Move0,BestValue,BestMove)
    ). 

minimax9([Move|Moves],Tablero,D,Player1, min, Value0,Move0,BestValue,BestMove):-
    mover(Move, Tablero, Tablero2), 
    obtenerRival(Player1, Player2),

    %minimax6(D, Tablero2, Player2, maxim, OppValue, OppMove), 
    minimax6(D, Tablero2, Player2, maxim, OppValue, _), 
    
    ( OppValue < Value0 ->        
        minimax9(Moves,Tablero,D,Player1, minim, OppValue ,Move ,BestValue,BestMove)
    ; minimax9(Moves,Tablero,D,Player1, minim, Value0,Move0,BestValue,BestMove)
    ). 
