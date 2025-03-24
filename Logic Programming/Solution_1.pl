%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

tablero(0,_,[]).
tablero(F, C, [Fila | T]) :- F > 0, F1 is F - 1, tablero(F1, C, T), generar_fila(C, Fila).

%generar_fila(+C,-L).
generar_fila(0, []).
generar_fila(C, [_ | Fila]) :- C > 0, C1 is C-1, generar_fila(C1, Fila).


%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.

ocupar(pos(X, Y), T) :- nonvar(T), nth0(X, T, Fila), nth0(Y, Fila, ocupada).
ocupar(pos(X, Y), T) :- var(T), generarPares(F,C), tablero(F,C,T), ocupar(pos(X,Y),T).

%Para el caso de T no instanciada, generamos tableros infinitos que cumplan tener la pos(X,Y) ocupada.
%Predicados definidos en la práctica para generacion infinita:
%desde(+X,-Y).
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

%paresSuman(+S,?X,?Y).
paresSuman(S,X,Y) :- S1 is S-1, between(1,S1,X), Y is S-X. 

%generarPares(?X,?Y). No pueden estar instanciados los dos a la vez, o se cuelga.
generarPares(X,Y) :- desde(2,S), paresSuman(S,X,Y).


%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

tablero(ej5x5, T) :- tablero(5, 5, T), ocupar(pos(1,1), T), ocupar(pos(1, 2), T).

vecino(pos(X,Y), T, pos(X1, Y)) :- length(T, F), X1 is X + 1, X1 >= 0, F > X1. 
vecino(pos(X,Y), T, pos(X, Y1)) :- nth0(0, T, Fila), length(Fila, C), Y1 is Y + 1, Y1 >= 0, C > Y1. 
vecino(pos(X,Y), _, pos(X1, Y)) :- X1 is X - 1, X1 >= 0. 
vecino(pos(X,Y), _, pos(X, Y1)) :- Y1 is Y - 1, Y1 >= 0. 


%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero

%%Utilizamos la tecnica de generate and test, pues generamos todos los vecinos y luego testeamos que estén libres:
vecinoLibre(P, T, pos(X, Y)) :- vecino(P, T, pos(X, Y)), posLibreValida(pos(X,Y),T).

%%posLibreValida(?P,?T)
posLibreValida(pos(X,Y),T) :- nth0(X, T, Fila), nth0(Y, Fila, Celda), var(Celda).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas

camino(Inicio,Fin,T,Camino) :- posLibreValida(Inicio,T), posLibreValida(Fin,T), caminoAux(Inicio,Fin,T,Camino,[Inicio]).

%caminoAux(+Inicio, +Fin, +Tablero, -Camino, +Visitados)
caminoAux(Fin,Fin,_,[Fin],[Fin|_]).
caminoAux(Inicio,Fin,T,[Inicio|Camino],[Inicio|Recorridos]) :- 
      Inicio \= Fin, 
      not(member(Inicio, Recorridos)), 
      vecinoLibre(Inicio, T, V), 
      caminoAux(V, Fin, T, Camino, [V,Inicio|Recorridos]).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

%% Análisis de reversibilidad para Fin:
%% Si Fin no está instanciada, será instanciada por posLibreValida(Fin,T), predicado que está definido con el uso de nth0(?Index, ?List, ?Elem),
%% que arrojará valores válidos para cada par índice y elemento de la lista. 
%% En conclusión, Fin tomará todos los valores posibles dentro del rango de nuestro tablero que cumplan no estar ocupados y luego funcionará correctamente.

%% Análisis de reversibilidad para Camino:
%% Si Camino está instanciada, se tendrá éxito si Camino es algún camino válido, pues unificará con alguno de los caminos que caminoAux vaya generando
%% (la cantidad de caminos generados es finita).
%% En conclusión, es una comparacion entre dos listas cuyos elementos son variables instanciadas, lo cual funciona correctamente.


%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

%Utilizamos la tecnica de generate and test. Generamos numeros entre 1 y N (la long maxima de un camino), generamos todos los caminos posibles
%y luego corroboramos y nos quedamos con los que cumplan la longitud, respetando así el orden.

camino2(Inicio,Fin,[T|Ts],Camino) :- 
      length(T,C), length([T|Ts],F), N is F*C, 
      between(1,N,Long), 
      camino(Inicio,Fin,[T|Ts],Camino), 
      length(Camino, N2), Long is N2.

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.

%% Análisis de reversibilidad para Inicio:
%% Idem análisis para Fin en el Ejercicio 5.1, pues el predicado camino2 utiliza también el predicado camino. Luego, posLibreValida se encargará de generar 
%% (gracias a nth0) todos los inicios validos dentro del rango del tablero instanciado.
%% Además, las soluciones se seguirán instanciando en orden creciente, ya que primero se generan todos los valores de Inicio posibles, luego sus caminos,
%% y finalmente se corrobora la longitud del resultado, respetando así el orden.

%% Análisis de reversibilidad para Camino:
%% Idem análisis para Camino en el Ejercicio 5.1, pues el predicado camino2 utiliza también el predicado camino. 
%% Luego, el predicado tendrá éxito si se instancia un Camino válido.


%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.

%Utilizamos la tecnica de generate and test. Generamos todos los caminos posibles y luego verificamos que no exista otro de menor longitud.
caminoOptimo(Inicio,Fin,T,Camino) :- camino(Inicio,Fin,T,Camino), length(Camino, N), not((camino(Inicio,Fin,T,Camino2), length(Camino2, N2), N2 < N)).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.

%Utilizamos la tecnica de generate and test. Generamos todos los caminos para T1 y despues para T2, luego verificamos que sean iguales.
caminoDual(Inicio,Fin,T1,T2,Camino) :- camino(Inicio, Fin, T1, C1), camino(Inicio, Fin, T2, Camino), C1 = Camino.


%%%%%%%%
%% TESTS
%%%%%%%%

cantidadTestsTablero(4).
testTablero(1) :- tablero(0,0,[]).
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
testTablero(3) :- tablero(3,3,[[_,_,_],[_,_,_],[_,_,_]]). %Crear tablero 3x3
testTablero(4) :- tablero(2,2,T), ocupar(pos(0,0),T), ocupar(pos(1,1),T), T = [[ocupada,_],[_,ocupada]]. %Ocupar varios

cantidadTestsVecino(4).
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
testVecino(2) :- %Devuelve todos los vecinos
      tablero(3,3,T), setof(V, vecino(pos(1,1), T, V), L),
      L = [pos(0, 1), pos(1, 0), pos(1, 2), pos(2, 1)].
testVecino(3) :- %Devuelve todos los vecinos en rango
      tablero(2,2,T), findall(V, vecino(pos(1,1),T,V), L), L = [pos(0,1),pos(1,0)].
testVecino(4) :- %Solo devuelve los vecinos libres.
      tablero(2,2,T), ocupar(pos(0,0),T), findall(V, vecinoLibre(pos(0,1),T,V), L), L = [pos(1,1)].

cantidadTestsCamino(10). 
testCamino(1) :- %Unico camino posible.
      tablero(2,2,T), ocupar(pos(0,1),T), camino(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0), pos(1,1)]). 
testCamino(2) :- %Multiples caminos
      tablero(3,3,T), ocupar(pos(1,1),T), 
      setof(C, camino(pos(0,0),pos(2,2),T,C), L),
      L = [[pos(0, 0), pos(0, 1), pos(0, 2), pos(1, 2), pos(2, 2)], [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2)]].
testCamino(3) :- %Inicio ocupado da false.
      tablero(2,2,T), ocupar(pos(0,1),T), not(camino(pos(0,1), pos(1,1), T, _)).
testCamino(4) :- %Fin ocupado da false.
      tablero(2,2,T), ocupar(pos(0,1),T), not(camino(pos(0,0), pos(0,1), T, _)).
testCamino(5) :- %Inicio fuera de rango da false.
      tablero(2,2,T), not(camino(pos(3,3), pos(0,1), T, _)).
testCamino(6) :- %Fin fuera de rango da false.
      tablero(2,2,T), not(camino(pos(0,1), pos(3,3), T, _)).
testCamino(7) :- %Si no existe camino posible, da false
      tablero(1,4,T), ocupar(pos(0,1),T), not(camino(pos(0,0), pos(0,3), T, _)).
testCamino(8) :- %Ejemplo del tp 5x5 da 287
      tablero(ej5x5,T), setof(C, camino(pos(0,0),pos(2,3),T,C), L), length(L, 287).
testCamino(9) :- %Caminos ordenados de menor a mayor longitud
      tablero(3,3,T), ocupar(pos(0,2),T),
      findall(C, camino2(pos(0,0),pos(2,2),T,C), L),
      caminosOrdenados(L).
testCamino(10) :- %Ejemplo del tp 5x5 da 287 y no se pierden resultados con camino2
      tablero(ej5x5,T), setof(C, camino2(pos(0,0),pos(2,3),T,C), L), length(L, 287).

caminosOrdenados([
      [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2)],
      [pos(0, 0), pos(1, 0), pos(1, 1), pos(2, 1), pos(2, 2)],
      [pos(0, 0), pos(1, 0), pos(1, 1), pos(1, 2), pos(2, 2)],
      [pos(0, 0), pos(0, 1), pos(1, 1), pos(2, 1), pos(2, 2)],
      [pos(0, 0), pos(0, 1), pos(1, 1), pos(1, 2), pos(2, 2)],
      [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(1, 1), pos(1, 2), pos(2, 2)],
      [pos(0, 0), pos(0, 1), pos(1, 1), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2)]
]).

cantidadTestsCaminoOptimo(2).
testCaminoOptimo(1) :- %Solo devuelve los optimos
      tablero(3,3,T), ocupar(pos(0,2),T),
      findall(C, caminoOptimo(pos(0,0),pos(2,2),T,C), L),
      caminosOrdenadosOptimos(L).
testCaminoOptimo(2) :- %Ejemplo del tp 5x5 da 2
      tablero(ej5x5,T), setof(C, caminoOptimo(pos(0,0),pos(2,3),T,C), L), length(L, 2).

caminosOrdenadosOptimos([
      [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2)],
      [pos(0, 0), pos(1, 0), pos(1, 1), pos(2, 1), pos(2, 2)],
      [pos(0, 0), pos(1, 0), pos(1, 1), pos(1, 2), pos(2, 2)],
      [pos(0, 0), pos(0, 1), pos(1, 1), pos(2, 1), pos(2, 2)],
      [pos(0, 0), pos(0, 1), pos(1, 1), pos(1, 2), pos(2, 2)]
]).

cantidadTestsCaminoDual(5).
testCaminoDual(1) :- %Unico camino posible
      tablero(2,2,T1), tablero(2,2,T2), ocupar(pos(1,0),T2),
      setof(C, caminoDual(pos(0,0), pos(1,1), T1, T2, C), L),
      L = [[pos(0,0), pos(0,1), pos(1,1)]].
testCaminoDual(2) :- %Más de un camino posible
      tablero(2,2,T1), tablero(2,2,T2),
      setof(C, caminoDual(pos(0,0), pos(1,1), T1, T2, C), L),
      L = [[pos(0,0), pos(0,1), pos(1,1)], [pos(0,0), pos(1,0), pos(1,1)]].
testCaminoDual(3) :- %Ningun camino posible
      tablero(2,2,T1), tablero(2,2,T2),
      ocupar(pos(0,1),T1), ocupar(pos(1,0),T2),
      not(setof(C, caminoDual(pos(0,0), pos(1,1), T1, T2, C), _)).
testCaminoDual(4) :- %Tableros de distintos tamaños, posiciones en rango para el mas chico
      tablero(ej5x5,T1), tablero(3,3,T2),
      setof(C, caminoDual(pos(0,0), pos(2,0), T1, T2, C), L),
      L = [[pos(0,0),pos(1,0),pos(2,0)]].
testCaminoDual(5) :- %Tableros de distintos tamaños, posiciones en rango para el mas grande (no existe camino)
      tablero(ej5x5,T1), tablero(3,3,T2),
      not(setof(C, caminoDual(pos(0,0), pos(4,4), T1, T2, C), _)).


tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).

tests(todos) :-
  tests(tablero),
  tests(vecino),
  tests(camino),
  tests(caminoOptimo),
  tests(caminoDual).

tests :- tests(todos).