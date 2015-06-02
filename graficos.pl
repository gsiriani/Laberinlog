% Predicados gráficos para la segunda tarea de 
% Programación Lógica 2015 - LaberinLog
% 
% Este archivo no es entregable, así que no debe ser modificado, salvo
% para ayudar a depurar. A medida que se encuentren errores y se
% sugieran mejoras van a haber nuevas versiones.
%
%

:- module(graficos,
[
 gr_crear/2, % -Ventana +Botones
 % Devuelve un handle a la Ventana creada.
 % En botones viene una lista de boton(Nombre, Evento)
 % indicando los botones a crear.
 
 gr_destruir/1, % +Ventana
 % Cierra la ventana e invalida el handle.
 
 gr_dibujar_tablero/3, % +Ventana, +Piezas, +PiezaSobra
 % Redibuja el tablero con una nueva lista de piezas.
 % * Piezas contiene el conjunto de piezas del tablero, representado como una lista de filas, donde
 %   cada fila es una lista de casilleros. Cada casillero se representa de la siguiente manera:
 %      casillero(InfoPieza,Jugadores)
 %          * InfoPieza representa la pieza que está ubicada en ese casillero, y se representa de la
 %            siguiente manera:
 %                  pieza(Valor,Topologia,Direccion)
 %                      * Valor es el valor de la pieza (el nombre de un objetivo o el atomo no)
 %                      * Topologia es la forma de la pieza: t, l, i
 %                      * Direccion es la direccion en que esta apuntando la pieza: n, e, s, o
 %                  Ejemplo de InfoPieza: pieza(buho, l, s)
 %          * Jugadores es una lista que contiene los identificadores de los jugadores que se
 %            encuentran en ese momento en el casillero:
 %                  Ejemplo de Jugadores: [verde, rojo]
 % * Pieza representa la pieza que sobra para que el siguiente jugador realice un shift. Se
 %   representa mediante un InfoPieza.

 gr_dibujar_objetivos/2, % +Ventana, +Objetivos
 % Dibuja un mazo de objetivos.
 % Objetivos es una lista de nombres de objetivos

 gr_evento/2, % +Ventana ?Evento
 % Devuelve en Evento la acción del usuario, que puede ser 
 % click(Fila, Col), salir o
 % el del boton accionado.

 gr_mensaje/2, % +Ventana +String
 % Muestra String en una ventanita auxiliar,
 % quedando la aplicación a la espera de que ésta se cierre.

 gr_pregunta/3, % +Ventana +Pregunta ?Respuesta
 % Muestra una ventanita conteniendo Pregunta
 % y un espacio para que el usuario ingrese Respuesta.
 % Se regresa del predicado cuando el usuario selecciona el botón.
 % El predicado falla si se cierra el dialogo.
 
 gr_opciones/4, % +Ventana +Pregunta +Opciones ?Respuesta
 % Muestra una ventanita conteniendo Pregunta
 % y un botón por cada elemento de Opciones,
 % para que elija el usuario.
 % Se regresa del predicado cuando el usuario selecciona un botón,
 % devolviendo el elegido en Respuesta
 % El predicado falla si se cierra el dialogo.

 gr_estado/2 , % +Ventana +NuevoEstado
 % Muestra el NuevoEstado de la partida en la parte inferior 
 % de la pantalla.

 gr_purgar/0
 % Cierra todas las ventanas que pueden haber quedado abiertas
 % por fallos del programa. Hack, usar solo en desarrollo.
]).


:- use_module(library(tabular)).
:- use_module(library(autowin)).
:- use_module(library(dragdrop)).

%La clase root.
:- pce_begin_class(my_frame, frame).

variable(queue, any, both).
variable(image, image, both).
variable(dialog, dialog, both).

%Desactivamos que el usuario la cierre.
%En su lugar, mandamos un mensaje salir.
wm_delete(Self) :->
	get(Self, queue, Queue),
	thread_send_message(Queue, salir).

:- pce_end_class.

tam_casilla(60, 60).

offset_casilla(6, 6).

fila_columna(X,Y,I,J):-
    tam_casilla(TX, TY),
    offset_casilla(OX, OY),
    J is 1 + (X - OX) // TX,
    I is 1 + (Y - OY) // TY.

click_tablero(Q, Punto) :-
	get(Punto, y, Y),
	get(Punto, x, X),
    get_evento_coords(X,Y,Evento),
	thread_send_message(Q, Evento).

get_evento_coords(X, Y, click(F,C)):-
    X =< 426,
    Y =< 426,!,
	fila_columna(X, Y, F, C).
get_evento_coords(X, _, rotar_izq):-
    X =< 50,!.
get_evento_coords(X, _, rotar_der):-
    X =< 100,!.
get_evento_coords(_, _, invalido).
    
:- send(class(my_frame), record_instances).

gr_purgar :-
	get(class(my_frame), instances, I),
	send(I, for_all, message(@arg1, destroy)).

gr_crear(Frame, Botones) :-
	message_queue_create(Q),
	new(Frame, my_frame('LaberinLog')),
	new(W, auto_sized_dialog),
	send(Frame, can_resize,	@off),
	forall(member(boton(Txt, Val), Botones),
	       send(W, append, 
		    button(Txt, 
			   message(@prolog,
				   thread_send_message,
				   prolog(Q),
				   prolog(Val))))),
	send(W, max_size, size(1000, 1200)),
	new(I, image(kind := pixmap)),
	new(B, bitmap(I)),
	send(B, recogniser, 
	     click_gesture(left, '', single, 
			      message(@prolog,
				      click_tablero,
				      prolog(Q),
				      @event?position))),
	send(W, append, B),
	send(W, append, label(reporter)),
	send(Frame, append, W),
	send(Frame, queue, prolog(Q)),
	send(Frame, image, I),
	send(Frame, dialog, W),
	gr_dimensiones(Frame),
	send(Frame, open).

gr_destruir(Ventana) :-
	get(Ventana, queue, Q),
	message_queue_destroy(Q),
	send(Ventana, destroy).

gr_dimensiones(Ventana) :-
	get(Ventana, image, I),
	send(I, resize, 484, 494),
	get(Ventana, dialog, W),
	send(W, redraw),
    gr_dibujar(Ventana,0,0,tablero,gif),
	!.

gr_dibujar(Ventana, X, Y, Imagen, Ext):-
    atomic_list_concat(['graficos/', Imagen, '.', Ext], Arch),
	new(ImgFicha,image(Arch)),
	get(Ventana, image, I),
	send(I, draw_in, bitmap(ImgFicha), point(X, Y)),
	send(Ventana, flush),
	!.
    
gr_dibujar_objetivos(Ventana,Objetivos):-
    gr_dibujar(Ventana,230,432,'vacio',jpg),
    gr_dibujar_objetivos(Ventana,230,432,Objetivos).

gr_dibujar_objetivos(_,_,_,[]).
gr_dibujar_objetivos(Ventana,X,Y,[Obj|Objetivos]):-
    XO is X + 3,
    gr_dibujar_objetivos(Ventana,XO,Y,Objetivos),
    atomic_list_concat(['o_',Obj],Archivo),
    gr_dibujar(Ventana,X,Y,Archivo,jpg).

gr_dibujar_tablero(Ventana, Piezas, pieza(Valor,Topologia,DirPieza)):-
    gr_dibujar_filas(Ventana, 0, Piezas),
    obtener_grafico_pieza(Valor,Topologia,DirPieza,Archivo),
    gr_dibujar(Ventana,19,432,Archivo,jpg).

gr_dibujar_filas(_,_,[]).
gr_dibujar_filas(Ventana,I,[Fila|Resto]):-
    gr_dibujar_fila(Ventana,I,0,Fila),
    I1 is I + 1,
    gr_dibujar_filas(Ventana,I1,Resto).

gr_dibujar_fila(_,_,_,[]).
gr_dibujar_fila(Ventana,I,J,[casillero(pieza(Valor,Topologia,DirPieza),Jugadores)|Resto]):-
    X is J * 60 + 6,
    Y is I * 60 + 6,
    obtener_grafico_pieza(Valor,Topologia,DirPieza,Archivo),
    gr_dibujar(Ventana,X,Y,Archivo,jpg),
    gr_dibujar_jugadores(Ventana,X,Y,Jugadores),
    J1 is J + 1,
    gr_dibujar_fila(Ventana,I,J1,Resto).

gr_dibujar_jugadores(_,_,_,[]).
gr_dibujar_jugadores(Ventana,X,Y,[Color|Resto]):-
    XO is X + 5,
    YO is Y - 1,
    gr_dibujar_jugadores(Ventana,XO,YO,Resto),
    atom_concat('p_',Color,Archivo),
    gr_dibujar(Ventana,X,Y,Archivo,gif).

obtener_grafico_pieza(no,Topologia,DirPieza,Archivo):-
    !,
    atomic_list_concat([Topologia,'_',DirPieza],Archivo).
obtener_grafico_pieza(Valor,Topologia,DirPieza,Archivo):-
    atomic_list_concat([Topologia,'_',DirPieza,'_',Valor],Archivo).

gr_evento(Ventana, Input) :-
	get(Ventana, queue, Q),
	thread_get_message(Q, Aux),
	!,
	Input = Aux.

gr_mensaje(V, Texto) :-
	new(D, dialog('Mensaje')),
	send(D, transient_for, V),
	send(D, append, label(lab, Texto)),
	send(D, append, button(ok,
			       message(D, return, @nil))),
	send(D, default_button, ok), % Ok: default button
	(   get(D, confirm, _Answer) % This blocks!
	->  send(D, destroy)
	;   true
	).

gr_pregunta(V, Preg, Resp) :-
	new(D, dialog('Pregunta')),
	send(D, transient_for, V),
        send(D, append,
             label(lab, Preg)),
	send(D, append,
             new(TI, text_item('', ''))),
        send(D, append,
             button(ok, message(D, return,
                                TI?selection))),
        send(D, default_button, ok), % Ok: default button
        get(D, confirm, Answer),     % This blocks!
        send(D, destroy),
	Answer = Resp.

gr_opciones(V, Texto, Opciones, Resp) :-
	new(D, dialog('Opciones')),
	send(D, transient_for, V),
	send(D, append, label(lab, Texto)),
	forall(member(O, Opciones),
	       send(D, append, button(O,
			       message(D, return, O)))),
	get(D, confirm,Answer),
	send(D, destroy),
	Resp = Answer.

gr_estado(MV,NuevoEstado) :-
	send(MV, report, progress,'%s',NuevoEstado).
