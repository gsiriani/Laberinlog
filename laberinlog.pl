:- use_module(graficos).
:- ensure_loaded(def).

% ---------------------
% Loop principal
% ---------------------

 % laberinlog/3, % +JugadorVerde, +JugadorRojo, +MaxObjetivos
 % Predicado principal del juego
 % * JugadorVerde y JugadorRojo pueden valer humano|maquina
 % * MaxObjetivos es la cantidad de objetivos a repartir a cada jugador
laberinlog(_JugadorVerde,_JugadorRojo,_MaxObjetivos):-
    gr_crear(Ventana,[boton('Salir',salir)]),
    loop(Ventana),
    gr_destruir(Ventana).

 % loop/1, % +Ventana
 % Loop principal del juego
loop(Ventana):-
    armarTablero(Tablero,PiezaExtra),
    gr_dibujar_tablero(Ventana, Tablero, PiezaExtra),
    gr_dibujar_objetivos(Ventana,[buho]),
	gr_evento(Ventana,E),!,
    procesar_evento(E,Ventana).

 % procesar_evento/2, % +Evento, +Ventana
 % Atiende los eventos del usuario
procesar_evento(salir,_):-!.
procesar_evento(click(I,J),Ventana):-
    atomic_list_concat(['Click en fila ',I,', columna ', J],Texto),
    gr_estado(Ventana,Texto),
    loop(Ventana).
procesar_evento(rotar_izq,Ventana):-!,
    gr_estado(Ventana,'Rotar a la izquierda'),
    loop(Ventana).
procesar_evento(rotar_der,Ventana):-!,
    gr_estado(Ventana,'Rotar a la derecha'),
    loop(Ventana).
procesar_evento(_,Ventana):-!,
    loop(Ventana).

% armarTablero/2, % -Tablero, -PiezaExtra
% Crea un tablero de juego inicial y retorna la pieza PiezaExtra
armarTablero([
    [casillero(F1,[verde]),casillero(M1,[]),casillero(F2,[]),casillero(M2,[]),casillero(F3,[]),casillero(M3,[]),casillero(F4,[rojo])],
    [casillero(M4,[]),casillero(M5,[]),casillero(M6,[]),casillero(M7,[]),casillero(M8,[]),casillero(M9,[]),casillero(M10,[])],
    [casillero(F5,[]),casillero(M11,[]),casillero(F6,[]),casillero(M12,[]),casillero(F7,[]),casillero(M13,[]),casillero(F8,[])],
    [casillero(M14,[]),casillero(M15,[]),casillero(M16,[]),casillero(M17,[]),casillero(M18,[]),casillero(M19,[]),casillero(M20,[])],
    [casillero(F9,[]),casillero(M21,[]),casillero(F10,[]),casillero(M22,[]),casillero(F11,[]),casillero(M23,[]),casillero(F12,[])],
    [casillero(M24,[]),casillero(M25,[]),casillero(M26,[]),casillero(M27,[]),casillero(M28,[]),casillero(M29,[]),casillero(M30,[])],
    [casillero(F13,[]),casillero(M31,[]),casillero(F14,[]),casillero(M32,[]),casillero(F15,[]),casillero(M33,[]),casillero(F16,[])]],
    PiezaExtra):-
    piezas_fijas_orientadas([F1,F2,F3,F4,F5,F6,F7,F8,
        F9,F10,F11,F12,F13,F14,F15,F16]),
    piezas_movibles(Moviles),
    random_permutation(Moviles,Permutadas),
    orientar_random(Permutadas,Orientadas),
    Orientadas=[M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,
                M11,M12,M13,M14,M15,M16,M17,M18,M19,M20,
                M21,M22,M23,M24,M25,M26,M27,M28,M29,M30,
                M31,M32,M33,PiezaExtra].

% piezas_fijas_orientadas/1 es la lista de piezas que tienen posiciones fijas en el tablero con su orientacion
piezas_fijas_orientadas([
    pieza(no,l,e),
    pieza(anillo,t,s),
    pieza(mapa,t,s),
    pieza(no,l,s),
    pieza(candelabro,t,e),
    pieza(cofre,t,e),
    pieza(corona,t,s),
    pieza(libro,t,w),
    pieza(yelmo,t,e),
    pieza(gema,t,n),
    pieza(llave,t,w),
    pieza(oro,t,w),
    pieza(no,l,n),
    pieza(espada,t,n),
    pieza(calavera,t,n),
    pieza(no,l,w)
]).

% piezas_movibles/1 es la lista de piezas para repartir aleatoriamente en el tablero
% Cada pieza tiene el formato pieza(Figura, Topologia)
% El átomo "no" se utiliza para indicar que la pieza no tiene una figura asociada
piezas_movibles([
    pieza(aranya,l),
    pieza(murcielago,t),
    pieza(escarabajo,l),
    pieza(libelula,l),
    pieza(hada,t),
    pieza(genio,t),
    pieza(lagarto,l),
    pieza(dragon,t),
    pieza(buho,l),
    pieza(duende,t),
    pieza(rata,l),
    pieza(fantasma,t),
    pieza(no,i),
    pieza(no,i),
    pieza(no,i),
    pieza(no,i),
    pieza(no,i),
    pieza(no,i),
    pieza(no,i),
    pieza(no,i),
    pieza(no,i),
    pieza(no,i),
    pieza(no,i),
    pieza(no,i),
    pieza(no,l),
    pieza(no,l),
    pieza(no,l),
    pieza(no,l),
    pieza(no,l),
    pieza(no,l),
    pieza(no,l),
    pieza(no,l),
    pieza(no,l),
    pieza(no,l)
]).

% orientar_random/2, % +PiezasSinOrientacion, -piezas_fijas_orientadas
% Retorna la lista de piezas luego de asignarle una orientacion aleatoria a cada una
orientar_random([pieza(Topologia,Figura)|T],[pieza(Topologia,Figura,Orientacion)|T2]):-
    random_select(Orientacion,[n,s,e,w],_),
    orientar_random(T,T2).
orientar_random([],[]).


