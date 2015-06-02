% Definiciones para la segunda tarea de 
% Programación Lógica 2015 - LaberinLog
% 
% Este archivo no es entregable, así que no debe ser modificado, salvo
% para ayudar a depurar.
%
%

% objetivos/1 es la lista de objetivos para los jugadores
objetivos([
    libelula,
    gema,
    anillo,
    corona,
    libro,
    cofre,
    hada,
    genio,
    duende,
    oro,
    rata,
    espada,
    fantasma,
    aranya,
    murcielago,
    llave,
    escarabajo,
    calavera,
    lagarto,
    dragon,
    candelabro,
    mapa,
    yelmo,
    buho
]).

% piezas_moviles/1 es la lista de piezas para repartir aleatoriamente en el tablero
% Cada pieza tiene el formato pieza(Topologa, Figura)
% El átomo "no" se utiliza para indicar que la pieza no tiene una figura asociada
piezas_moviles([
    pieza(l,aranya),
    pieza(t,murcielago),
    pieza(l,escarabajo),
    pieza(l,libelula),
    pieza(t,hada),
    pieza(t,genio),
    pieza(l,lagarto),
    pieza(t,dragon),
    pieza(l,buho),
    pieza(t,duende),
    pieza(l,rata),
    pieza(t,fantasma),
    pieza(i,no),
    pieza(i,no),
    pieza(i,no),
    pieza(i,no),
    pieza(i,no),
    pieza(i,no),
    pieza(i,no),
    pieza(i,no),
    pieza(i,no),
    pieza(i,no),
    pieza(i,no),
    pieza(i,no),
    pieza(l,no),
    pieza(l,no),
    pieza(l,no),
    pieza(l,no),
    pieza(l,no),
    pieza(l,no),
    pieza(l,no),
    pieza(l,no),
    pieza(l,no),
    pieza(l,no)
]).

% piezas_fijas/1 es la lista de piezas que tienen posiciones fijas en el tablero
% Cada pieza tiene el formato pieza(Topologa, Figura)
% El átomo "no" se utiliza para indicar que la pieza no tiene una figura asociada
piezas_fijas([
    pieza(l,no),
    pieza(t,anillo),
    pieza(t,mapa),
    pieza(l,no),
    pieza(t,candelabro),
    pieza(t,cofre),
    pieza(t,corona),
    pieza(t,libro),
    pieza(t,yelmo),
    pieza(t,gema),
    pieza(t,llave),
    pieza(t,oro),
    pieza(l,no),
    pieza(t,espada),
    pieza(t,calavera),
    pieza(l,no)
]).
