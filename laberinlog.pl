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
    armar_tablero(Tablero,PiezaExtra),
    repartir_objetivos(_MaxObjetivos,ObjetivosVerde,ObjetivosRojo),
    loop(Ventana,Tablero,PiezaExtra,ObjetivosVerde,ObjetivosRojo,shift),
    gr_destruir(Ventana).

 % loop/6, % +Ventana, +Tablero, +PiezaExtra, +ObjetivosVerde, +ObjetivosRojo, +Fase
 % Loop principal del juego
loop(Ventana,Tablero,PiezaExtra,ObjetivosVerde,ObjetivosRojo,Fase):-
    % turno jugador verde
    gr_dibujar_tablero(Ventana, Tablero, PiezaExtra),
    gr_dibujar_objetivos(Ventana,ObjetivosVerde),
	gr_evento(Ventana,E),!,
    procesar_evento(E,Ventana,Tablero,PiezaExtra,ObjetivosVerde,ObjetivosRojo,Fase).

 % procesar_evento/4, % +Evento, +Ventana, +Tablero, +PiezaExtra
 % Atiende los eventos del usuario
procesar_evento(salir,_,_,_,_,_,_):-!.
procesar_evento(click(I,J),Ventana,Tablero,PiezaExtra,ObjetivosVerde,ObjetivosRojo,shift):-
    atomic_list_concat(['Click en fila ',I,', columna ', J],Texto),
    gr_estado(Ventana,Texto),
    loop(Ventana,Tablero,PiezaExtra,ObjetivosVerde,ObjetivosRojo,movimiento).
procesar_evento(click(I,J),Ventana,Tablero,PiezaExtra,ObjetivosVerde,ObjetivosRojo,movimiento):-
    atomic_list_concat(['Click en fila ',I,', columna ', J],Texto),
    gr_estado(Ventana,Texto),
    loop(Ventana,Tablero,PiezaExtra,ObjetivosVerde,ObjetivosRojo,shift).
procesar_evento(rotar_izq,Ventana,Tablero,pieza(F,T,O),ObjetivosVerde,ObjetivosRojo,shift):-!,
    gr_estado(Ventana,'Rotar a la izquierda'),
    rotar_izquierda(O,O2),
    loop(Ventana,Tablero,pieza(F,T,O2),ObjetivosVerde,ObjetivosRojo,shift).
procesar_evento(rotar_der,Ventana,Tablero,pieza(F,T,O),ObjetivosVerde,ObjetivosRojo,shift):-!,
    gr_estado(Ventana,'Rotar a la derecha'),
    rotar_derecha(O,O2),
    loop(Ventana,Tablero,pieza(F,T,O2),ObjetivosVerde,ObjetivosRojo,shift).
procesar_evento(_,Ventana,Tablero,PiezaExtra,ObjetivosVerde,ObjetivosRojo,Evento):-!,
    loop(Ventana,Tablero,PiezaExtra,ObjetivosVerde,ObjetivosRojo,Evento).

% armar_tablero/2, % -Tablero, -PiezaExtra
% Crea un tablero de juego inicial y retorna la pieza PiezaExtra
armar_tablero([
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

% repartir_objetivos/3, % +MaxObjetivos, -Verde, -Rojo
% Reparte MaxObjetivos entre 2 listas
% Si MaxObjetivos es 0, reparte todos los objetivos
repartir_objetivos(MaxObjetivos,Verde,Rojo):-
    objetivos(Objetivos),
    repartir_objetivos_rec(MaxObjetivos,Objetivos,Verde,Rojo).
repartir_objetivos(0,Verde,Rojo):-
    objetivos(Objetivos),
    repartir_objetivos_rec(Objetivos,Verde,Rojo).


% repartir_objetivos_rec/4, % +Max, +Objetivos, -Verde, -Rojo
% Reparte aleatoriamente Max objetivos en las listas Verde y Rojo
repartir_objetivos_rec(0,_,[],[]).
repartir_objetivos_rec(Max,Objetivos,[HV|TV],[HR|TR]):-
    random_select(HV,Objetivos,Objetivos2),
    random_select(HR,Objetivos2,Objetivos3),
    X is Max - 1,
    repartir_objetivos_rec(X,Objetivos3,TV,TR).


% repartir_objetivos_rec/3, % +Objetivos, -Verde, -Rojo
% Reparte aleatoriamente todos los objetivos en las listas Verde y Rojo
repartir_objetivos_rec(_,[],[],[]).
repartir_objetivos_rec(Objetivos,[HV|TV],[HR|TR]):-
    random_select(HV,Objetivos,Objetivos2),
    random_select(HR,Objetivos2,Objetivos3),
    repartir_objetivos_rec(Objetivos3,TV,TR).


% rotar_derecha/2, % +Orientacion, -NuevaOrientacion
% NuevaOrientacion es el resultado de rotar a la derecha la Orientacion
rotar_derecha(s,w).
rotar_derecha(w,n).
rotar_derecha(n,e).
rotar_derecha(e,s).

% rotar_izquierda/2, % +Orientacion, -NuevaOrientacion
% NuevaOrientacion es el resultado de rotar a la izquierda la Orientacion
rotar_izquierda(s,e).
rotar_izquierda(e,n).
rotar_izquierda(n,w).
rotar_izquierda(w,s).