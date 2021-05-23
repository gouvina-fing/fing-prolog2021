# Arabe en Prolog
# Laboratorio Programaci�n L�gica 2021
# UdelaR

Esta interfaz permite jugar al �rabe, invocando a los predicados implentados en Prolog que implementan las reglas del juego y a los jugadores IA. Es posible jugar Humano contra Humano, Humano contra M�quina, y M�quina contra M�quina. Est� basada en [Prolog XO](https://github.com/guyzyl/prolog-tic-tac-toe), una soluci�n para ta-te-t� que utiliza Python para hacer el puente con SWI Prolog.

# Instalaci�n
- Asegurarse de tener instalado Python (3.6+). Recomendado: instalar la distribuci�n [Anaconda](https://www.anaconda.com/products/individual).
- Instalar las bibliotecas ejecutando `pip -m pip install requirements.txt`
- Asegurarse de que SWI Prolog est� en el Path del sistema
- Abrir una consola, y ejecutar `python server.py` para ejecutar el servidor del juego. Deber�a aparecer un mensaje similar a

* Serving Flask app "server" (lazy loading)
 * Environment: production
   WARNING: This is a development server. Do not use it in a production deployment.
   Use a production WSGI server instead.
 * Debug mode: on
 * Restarting with windowsapi reloader
 * Debugger is active!
 * Debugger PIN: 214-218-041
 * Running on http://0.0.0.0:5000/ (Press CTRL+C to quit)

- A partir de aqu�, ir a [http://localhost:5000/](http://localhost:5000/) para jugar.

IMPORTANTE: Si se modifica el fuente Prolog (arabelog.pl), se debe reiniciar el servidor. En los otros casos, basta con recargar la p�gina del juego. 

# Reglas del juego

El objetivo del juego es inmovilizar las fichas del oponente. Hay 12 fichas para cada jugador. Empiezas siempre las X. 

El juego se divide en dos etapas. En la primera se colocan las piezas. En su turno cada jugador colocar� dos fichas en cualquier parte del tablero menos en la casilla central, 
que en esta etapa del juego debe quedar desocupada. 

En la segunda etapa, las fichas pueden moverse en forma horizontal o vertical hasta una casilla vecina desocupada. No se puede mover en diagonal. Si una ficha es encerrada entre
dos fichas contrarias, es capturada y retirada inmediatamente del tablero. En una misma jugada se pueden realizar varias capturas. La captura en diagonal no est� permitida.

Turno extra: luego de capturar y retirar la o las fichas del oponente, el jugador que realiz� una captura toma un turno extra siempre que pueda efectuar otra captura. En caso de no poder capturar pierde su turn extra. 

Un jugador puede colocar su ficha entre dos fichas enemigas sin sufrir captura. La ficha que se encuentre en el centro del tablero no puede ser capturada mientras permanezca all�. 

Cuando un jugador no puede mover ninguna de sus fichas pierde su turno. Si luego de tres jugadas del oponente aun no puede mover, pierde el juego. 

Ser� declarado ganador aquel jugador que capture o inmovilice las fichas del oponente.

# La interfaz

En la interfaz del juego tenemos los siguiente componentes: 

- Un indicador para cada jugador que indica/permite seleccionar si es Humano o M�quina, en negro la cantidad de fichas que tiene en el tablero, y en rojo
los turnos seguidos que ha perdido por no poder mover, y la cantidad de movimientos sin capturar que lleva.
- Si el jugador es m�quina, permite seleccionar la estrategia, y el nivel para Minimax
- Un indicador de a qu� jugador le toca jugar
- Un indicador de la etapa en la que se est� jugando (1/Insertar fichas, 2/Mover fichas)
- Un bot�n "Jugar" para reiniciar el juego
- Un bot�n "Jugar IA" que solamente aparece cuando el jugador que tiene el turno es M�quina, y que permite hacer la mejor jugada.
- Un bot�n que solamente aparece cuando estamos en turno extra, que permite pasar si no se quiere hacer la captura
- El tablero. Cada casilla puede estar vac�a, tener una X o una O, y toma el valor especial - para cuando se est� en medio de un movimiento

Para insertar, basta hacer click en la casilla donde se quiere colocar la pieza.
Para mover, se hace click en la casilla origen (que cambiar� a "-") y luego click en la casilla destino, lo que completar� la jugada. 

# Archivos

index.html - La interfaz del juego (HTML+Javascript con vue.js)
server.py - Servidor web (usando Flask) que permite invocar (a trav�s de un bridge) los predicados Prolog necesarios para modificar la interfaz
prolog_bridge.py - Bridge entre Python y SWI Prolog. Los m�todos implementados en el bridge son:
arabelog.pl - Los predicados Prolog para el juego. 

Auxiliares:
- pyswip_mt.py: permite correr Prolog en un multithread. 
- requirements.txt: bibliotecas Python auxiliares
