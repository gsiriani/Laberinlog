:- module(funciones_matrices, 
	[
	valor_celda/4, % +I, +J, +Matriz, -Valor
	nuevo_valor_celda/5, % +I, +J, +Matriz, +Valor, -Resultado
	shift_fila/4, % +Fila, +DirShift (1=derecha, -1=izquierda), +M1, -M2
	shift_columna/4 % +Columna, +DirShift (1=abajo, -1=arriba), +M1, -M2
	]).

% Caso base: devolvemos el primer elemento, si coincide.
% Caso recursivo: removemos el primer elemento de ambas listas y hacemos el llamado con N-1
elemento_n(1,[E|R],E,R).
elemento_n(N,[H|T],E,[H|R]):-
	M is N-1,
	elemento_n(M,T,E,R).

% Obtenemos la fila indicada utilizando elemento_n y luego obtenemos el elemento de la celda dentro de esa fila.
% Obs: los indices de la matriz comienzan en 1
valor_celda(I,J,M,Val):-
	elemento_n(I,M,X,_),
	elemento_n(J,X,Val,_).


% Obs: los indices de la matriz comienzan en 1
nuevo_valor_celda(I,J,[L|M1],Val,M2):-
	obtener_matriz_aux(I,[],A,E,L,P,M1),
	nuevo_valor_celda_Ac(J,Val,E,[],L2),
	concatenar_inv(A,[L2|P],M2).

% Funciones auxiliares nuevo_valor_celda:
%
% --->obtener_matriz_aux(+I,+Ac,?A,?E,?P,+M) <- E es la fila I de la matriz M, P son las filas posteriores a la fila I y A son las filas anteriores a la fila I en orden inverso al de M.
%
% --->nuevo_valor_celda_Ac(+J, +Val, +E, +Ac, ?L2) <- L2 es la lista E con el valor de la posición J reemplazado por Val.
%
% --->concatenar_inv(+A,+P,?S) <- S es la lista inversa de A concatenada con la lista P. 

obtener_matriz_aux(1,A,A,F,F,M,M).
obtener_matriz_aux(I,Ac,A,E,F,P,[Y|M]):-
	I > 1,
	X is I - 1,
	obtener_matriz_aux(X,[F|Ac],A,E,Y,P,M).

nuevo_valor_celda_Ac(0,_,[],Ac,L2):-
	concatenar_inv(Ac,[],L2).
nuevo_valor_celda_Ac(0,Val,[X|L],Ac,L2):-
	nuevo_valor_celda_Ac(0,Val,L,[X|Ac],L2).
nuevo_valor_celda_Ac(1,Val,[_|L],Ac,L2):-
	nuevo_valor_celda_Ac(0,Val,L,[Val|Ac],L2).
nuevo_valor_celda_Ac(J,Val,[X|L],Ac,L2):-
	J > 1,
	Y is J - 1,
	nuevo_valor_celda_Ac(Y,Val,L,[X|Ac],L2).

% shift_fila/4, % +I, +DirShift, +M1, -M2
shift_fila(I,DirShift,M1,M2) :-
	shift_fila_Ac(I,DirShift,M1,[],M2).

% Funciones auxiliares shift_fila:
%
% --->shift_fila_Ac(+I,+DirShift,+M1,+Ac,?M2) <- M2 es la matriz M1 con la fila I desplazada en la dirección indicada
% por DirShift
%
% --->shift_der(+L,?S) <- S es la lista L desplazada a la derecha un lugar
% 
% --->shift_izq(+L,?S) <- S es la lista L desplazada a la izquierda un lugar

shift_fila_Ac(0,_,[],Ac,M2) :-
	concatenar_inv(Ac,[],M2).
shift_fila_Ac(0,_,[X|M],Ac,M2) :-
	shift_fila_Ac(0,_,M,[X|Ac],M2).
shift_fila_Ac(1,1,[X|M],Ac,M2) :-
	shift_der(X,L),
	shift_fila_Ac(0,_,M,[L|Ac],M2).
shift_fila_Ac(1,-1,[X|M],Ac,M2) :-
	shift_izq(X,L),
	shift_fila_Ac(0,_,M,[L|Ac],M2).
shift_fila_Ac(I,DirShift,[X|M],Ac,M2) :-
	I > 1,
	Y is I - 1,
	shift_fila_Ac(Y,DirShift,M,[X|Ac],M2).

shift_izq([],[]).
shift_izq([X|L],S):-
	concatenar_inv(L,[],Aux),
	concatenar_inv(Aux,[X],S),
	!.

shift_der(L,S):-
	concatenar_inv(L,[],Aux),
	shift_izq(Aux,P),
	concatenar_inv(P,[],S),
	!.

% shift_columna/4, % +J, +DirShift, +M1, -M2
shift_columna(J,DirShift,M1,M2):-
	obtener_nueva_columna(J,DirShift,M1,[],C),
	reemplazar_columna(J,C,M1,[],M2).

% Funciones auxiliares shift_columna:
%
% --->obtener_nueva_columna(+J,+DirShift,+M1,+Ac,?C) <- C es la columna J de M1 desplazada en la dirección
% indicada por DirShift
%
% --->obtener_celda(+J,+L,?X) <- X es el elemento J de la lista L1
%
% --->reemplazar_columna(+J,+C,+M1,+Ac,?M2) <- M2 es la matriz M1 con la columna J reemplazada por C
%
% --->nuevo_valor_celda_Rec(+J,+Val,+L,?L2) <- L2 es la lista L con el valor Val en el elemento J

obtener_nueva_columna(_,1,[],Ac,C):-
	concatenar_inv(Ac,[], Aux),
	shift_der(Aux,C).
obtener_nueva_columna(_,-1,[],Ac,C):-
	concatenar_inv(Ac,[], Aux),
	shift_izq(Aux,C).
obtener_nueva_columna(J,DirShift,[X|M1],Ac,C):-
	obtener_celda(J,X,Aux),
	obtener_nueva_columna(J,DirShift,M1,[Aux|Ac],C).

obtener_celda(1,[E|_], E).
obtener_celda(J,[_|L], X):-
	J > 1,
	G is J - 1,
	obtener_celda(G, L, X).

reemplazar_columna(_,[],[],Ac,M2):-
	concatenar_inv(Ac,[],M2).
reemplazar_columna(J,[E|C],[F|M1],Ac,M2):-
	nuevo_valor_celda_Rec(J,E,F,L),
	reemplazar_columna(J,C,M1,[L|Ac],M2).

nuevo_valor_celda_Rec(J,Val,[E|R],[E|R2]):-
	J > 1,
	X is J - 1,
	nuevo_valor_celda_Rec(X,Val,R,R2).
nuevo_valor_celda_Rec(1,Val,[_|R],[Val|R]).


concatenar_inv([],P,P):-
	!.
concatenar_inv([X|A],P,S):-
	concatenar_inv(A,[X|P],S),
	!.