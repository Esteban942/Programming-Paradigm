%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados básicos %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% proceso(+P)
proceso(computar).
proceso(escribir(_,_)).
proceso(leer(_)).
proceso(secuencia(P,Q)) :- proceso(P), proceso(Q).
proceso(paralelo(P,Q)) :- proceso(P), proceso(Q).

%% Ejercicio 2
%% buffersUsados(+P,-BS)

buffersUsados(computar,[]).
buffersUsados(escribir(B,_),[B]).
buffersUsados(leer(B),[B]).
buffersUsados(secuencia(P,Q),BS) :- buffersUsados(P,L1), buffersUsados(Q,L2), union(L1,L2,BS).
buffersUsados(paralelo(P,Q),BS) :- buffersUsados(P,L1), buffersUsados(Q,L2), union(L1,L2,BS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Organización de procesos %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 3
%% intercalar(+XS,+YS,?ZS)

intercalar([],[],[]).
intercalar([X|XS],[],[X|XS]).
intercalar([],[X|XS],[X|XS]).
intercalar([X|XS],[Y|YS],[X|ZS]) :- intercalar(XS,[Y|YS],ZS).
intercalar([X|XS],[Y|YS],[Y|ZS]) :- intercalar([X|XS],YS,ZS).

%% Ejercicio 4
%% serializar(+P,?XS)

serializar(computar,[computar]).
serializar(escribir(B,X),[escribir(B,X)]).
serializar(leer(B),[leer(B)]).
serializar(secuencia(P,Q),XS) :- serializar(P,L1), serializar(Q,L2), append(L1,L2,XS).
serializar(paralelo(P,Q),XS) :- serializar(P,L1), serializar(Q,L2), intercalar(L1,L2,XS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% contenidoBuffer(+B,+ProcesoOLista,?Contenidos)

contenidoBuffer(_,[],[]).
contenidoBuffer(B,[X|XS],Contenidos) :- contenidoBufferConHistorial(B,[X|XS],Contenidos,[]).
contenidoBuffer(B,Proceso,Contenidos) :- proceso(Proceso), serializar(Proceso,XS), contenidoBuffer(B,XS,Contenidos).

%% contenidoBufferConHistorial(+B,+Lista,?Contenidos,+Historial), donde recorremos la Lista y por cada escritura en B, agregamos su dato al final del Historial. 
%% Por cada lectura en B, eliminamos la primer escritura del Historial. Historial se debe instanciar con una lista vacia.
%% Se instancia en Contenidos el valor de Historial al terminar de recorrer la Lista.

contenidoBufferConHistorial(_,[],Z,Z).
contenidoBufferConHistorial(B,[escribir(B,Y)|XS],Contenidos,Escrituras) :-  append(Escrituras,[Y],ZS), 
                                                                            contenidoBufferConHistorial(B,XS,Contenidos,ZS).
contenidoBufferConHistorial(B,[escribir(X,_)|XS],Contenidos,Escrituras) :- X \= B, contenidoBufferConHistorial(B,XS,Contenidos,Escrituras).
contenidoBufferConHistorial(B,[computar|XS],Contenidos,Escrituras) :- contenidoBufferConHistorial(B,XS,Contenidos,Escrituras).
contenidoBufferConHistorial(B,[leer(B)|XS],Contenidos,[_|Escrituras]) :- contenidoBufferConHistorial(B,XS,Contenidos,Escrituras).
contenidoBufferConHistorial(B,[leer(X)|XS],Contenidos,Escrituras) :- X \= B, contenidoBufferConHistorial(B,XS,Contenidos,Escrituras).

%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista,?Contenidos)

contenidoLeido([],[]).
contenidoLeido([X|XS],Contenidos) :- leerContenidoPorBuffers([],[X|XS],Contenidos).
contenidoLeido(Proceso,Contenidos) :- proceso(Proceso), serializar(Proceso,Lista), contenidoLeido(Lista,Contenidos).

leerContenidoPorBuffers(_,[],[]).
leerContenidoPorBuffers(YS,[X|XS],Contenidos) :- X \= leer(_), append(YS,[X],ZS), leerContenidoPorBuffers(ZS,XS,Contenidos).
leerContenidoPorBuffers(YS,[leer(B)|XS],[D|Contenidos]) :- contenidoBuffer(B,YS,[D|_]), append(YS,[leer(B)],ZS), leerContenidoPorBuffers(ZS,XS,Contenidos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Secuencias y procesos seguros %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)

esSeguro(P) :- forall(serializar(P,XS), esEjecucionSegura(XS)), esSubprocesoParaleloSeguro(P).

%% esEjecucionSegura(+XS).
esEjecucionSegura(XS) :- contenidoLeido(XS,_).

%% esSubprocesoParaleloSeguro(+P). 
esSubprocesoParaleloSeguro(computar).
esSubprocesoParaleloSeguro(escribir(_,_)).
esSubprocesoParaleloSeguro(leer(_)).
esSubprocesoParaleloSeguro(secuencia(P,Q)) :- esSubprocesoParaleloSeguro(P), esSubprocesoParaleloSeguro(Q).
esSubprocesoParaleloSeguro(paralelo(P,Q)) :-  buffersUsados(P,BP), buffersUsados(Q,BQ), intersection(BP,BQ,[]), 
                                              esSubprocesoParaleloSeguro(P),
                                              esSubprocesoParaleloSeguro(Q).

%% Ejercicio 8
%% ejecucionSegura(-XS,+BS,+CS)

ejecucionSegura(XS,BS,CS) :- desde(0,N), generarListaDeProcesosDeLongitudN(XS,BS,CS,N), esEjecucionSegura(XS).

%% desde(+X,-Y). Instancia en Y todos los numeros desde X en adelante.
desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).

%% generarListaDeProcesosDeLongitudN(-XS,+BS,+CS,+N). 
generarListaDeProcesosDeLongitudN([],_,_,0).
generarListaDeProcesosDeLongitudN([P|XS],BS,CS,N) :- N > 0, generarProceso(P,BS,CS), N2 is N - 1, generarListaDeProcesosDeLongitudN(XS,BS,CS,N2).

%% generarProceso(-P,+BS,+CS). Genera un proceso en base a los buffers de BS y a los contenidos de CS.
generarProceso(computar,_,_).
generarProceso(escribir(B,C),BS,CS) :- member(B,BS), member(C,CS).
generarProceso(leer(B),BS,_) :- member(B,BS).

  %% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
  %% lo hace.

  %% Analisis:
  %% Si XS se instancia con una lista valida, el programa dará true al encontrar esa respuesta y unificarlas.
  %% Sin embargo, si pedimos que siga buscando respuestas, se colgará pues 'desde' genera infinitos numeros, generando listas de infinitos tamaños distintos.
  %% Como todas las listas que genere intentarán unificar con la instancia de XS sin éxito, el programa se colgará al comparar infinitas respuestas.
  %% Si XS se instancia con una lista invalida, el programa no dará false, sino que se colgará directamente por el mismo motivo de antes.
  %% En conclusion, XS no es reversible y la funcion debe ser llamada sin instanciarlo.


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

%% Predicados para utilizar procesos de ejemplo en los tests

procesoLargoParaTestBuffers(paralelo(secuencia(escribir(1, sol),secuencia(leer(2),escribir(3,agua))), secuencia(leer(4),computar))).

procesoParaTestSerializar(paralelo(paralelo(leer(1),leer(2)),secuencia(leer(3),leer(4)))).

procesoParaleloSolAgua(paralelo(secuencia(escribir(2,sol),leer(2)),secuencia(escribir(1,agua),leer(1)))).

procesoParaleloSolAguaMasLargo(paralelo(
                                  secuencia(escribir(2,sol),secuencia(leer(2),escribir(2,agua))),
                                  secuencia(leer(2),computar))).

%% Predicados para comparar con los resultados de algunos tests

intercalarTestResultado([ [1, 2, 3, 4, 5, 6], [1, 2, 4, 3, 5, 6], [1, 2, 4, 5, 3, 6], [1, 2, 4, 5, 6, 3], [1, 4, 2, 3, 5, 6], [1, 4, 2, 5, 3, 6],
                          [1, 4, 2, 5, 6, 3], [1, 4, 5, 2, 3, 6], [1, 4, 5, 2, 6, 3], [1, 4, 5, 6, 2, 3], [4, 1, 2, 3, 5, 6], [4, 1, 2, 5, 3, 6],
                          [4, 1, 2, 5, 6, 3], [4, 1, 5, 2, 3, 6], [4, 1, 5, 2, 6, 3], [4, 1, 5, 6, 2, 3], [4, 5, 1, 2, 3, 6], [4, 5, 1, 2, 6, 3],
                          [4, 5, 1, 6, 2, 3], [4, 5, 6, 1, 2, 3] ]).

serializarTestResultado([ [leer(1), leer(2), leer(3), leer(4)], [leer(1), leer(3), leer(2), leer(4)], [leer(1), leer(3), leer(4), leer(2)],
                          [leer(3), leer(1), leer(2), leer(4)], [leer(3), leer(1), leer(4), leer(2)], [leer(3), leer(4), leer(1), leer(2)],
                          [leer(2), leer(1), leer(3), leer(4)], [leer(2), leer(3), leer(1), leer(4)], [leer(2), leer(3), leer(4), leer(1)],
                          [leer(3), leer(2), leer(1), leer(4)], [leer(3), leer(2), leer(4), leer(1)], [leer(3), leer(4), leer(2), leer(1)] ]).

%% Tests

cantidadTestsBasicos(7).
testBasico(1) :- proceso(computar).
testBasico(2) :- proceso(secuencia(escribir(1,pepe),escribir(2,pipo))).
testBasico(3) :- proceso(paralelo(leer(2),leer(3))).
testBasico(4) :- not(proceso(paralelo(hola,chau))).
testBasico(5) :- buffersUsados(escribir(1, hola), [1]).
testBasico(6) :- buffersUsados(secuencia(leer(1), leer(1)), [1]).
testBasico(7) :- procesoLargoParaTestBuffers(P), buffersUsados(P, [1,2,3,4]).

cantidadTestsProcesos(4).
testProcesos(1) :- intercalar([1],[],[1]).
testProcesos(2) :- intercalarTestResultado(L), 
                   findall(XS, intercalar([1,2,3],[4,5,6],XS), L).
testProcesos(3) :- serializar(secuencia(computar,leer(2)), [computar,leer(2)]).
testProcesos(4) :- serializarTestResultado(L),
                   procesoParaTestSerializar(S),
                   findall(XS, serializar(S,XS), L).

cantidadTestsBuffers(8).
testBuffers(1) :- contenidoBuffer(1, [escribir(1,pa),escribir(2,ma),escribir(1,hola),computar,escribir(1,mundo),leer(1)], [hola, mundo]).
testBuffers(2) :- contenidoBuffer(2, [escribir(1,pp),escribir(2,ala),escribir(1,ola),computar,escribir(1,mundo),leer(1)], [ala]).
testBuffers(3) :- findall(XS, contenidoBuffer(2, paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1))), XS), [[sol],[sol],[sol]]).
testBuffers(4) :- findall(XS, contenidoBuffer(1, paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1))), XS), [[],[],[]]).
testBuffers(5) :- findall(XS, contenidoBuffer(1, paralelo(leer(1),escribir(1,agua)), XS), [[]]).
testBuffers(6) :- procesoParaleloSolAgua(P),
                  findall(XS, contenidoLeido(P,XS), [[sol,agua],[sol,agua],[agua,sol],[sol,agua],[agua,sol],[agua,sol]]).
testBuffers(7) :- not(contenidoLeido([escribir(1, agua), escribir(2, sol), leer(1), leer(1)], _)).
testBuffers(8) :- procesoParaleloSolAguaMasLargo(P), findall(XS, contenidoLeido(P,XS), [[sol,agua]]).

cantidadTestsSeguros(13).
testSeguros(1) :- esSeguro(escribir(1,hola)).
testSeguros(2) :- esSeguro(computar).
testSeguros(3) :- not(esSeguro(leer(1))).
testSeguros(4) :- not(esSeguro(secuencia(leer(1),escribir(1,agua)))).
testSeguros(5) :- esSeguro(secuencia(escribir(1,agua),leer(1))).
testSeguros(6) :- not(esSeguro(paralelo(escribir(1,sol),secuencia(escribir(1,agua),leer(1))))).
testSeguros(7) :- esSeguro(paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1)))).
testSeguros(8) :- ejecucionSegura([computar, escribir(1,a), escribir(2,b), leer(1), computar], [1,2], [a,b]), !.
testSeguros(9) :- ejecucionSegura([computar, computar, computar, escribir(1,hola), leer(1), escribir(1,hola), computar],[1],[hola]), !.
testSeguros(10) :- ejecucionSegura([computar, escribir(3,hola), leer(3), escribir(2, hola), computar, escribir(1,chau), leer(1)],[1,2,3],[hola,chau]), !.
testSeguros(11) :- ejecucionSegura([computar, computar, computar, computar, computar], [1], [a]), !.
testSeguros(12) :- ejecucionSegura([computar, computar, computar, computar, computar], [], [b]), !.
testSeguros(13) :- ejecucionSegura([computar, computar, computar, computar, computar, computar, computar], [2], []), !.

tests(basico) :- cantidadTestsBasicos(M), forall(between(1,M,N), testBasico(N)).
tests(procesos) :- cantidadTestsProcesos(M), forall(between(1,M,N), testProcesos(N)).
tests(buffers) :- cantidadTestsBuffers(M), forall(between(1,M,N), testBuffers(N)).
tests(seguros) :- cantidadTestsSeguros(M), forall(between(1,M,N), testSeguros(N)).

tests(todos) :-
  tests(basico),
  tests(procesos),
  tests(buffers),
  tests(seguros).

tests :- tests(todos).