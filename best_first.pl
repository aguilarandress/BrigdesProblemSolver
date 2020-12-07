/**
 * Andres Esteban Aguilar Moya - 2019156214
 * Lenguajes de Programacion - Tarea Programada 2
 */

/**
 * solve_best(Frontera, Historial, Movidas).
 * Frontera es una lista de puntos que deben ser explorados
 * Historial son los puntos explorados
 * Movidas son las movidas necesarias para llegar al estado final
 */
solve_best([punto(Estado, Ruta, _)|_], _, Movidas) :-
  % Si se llega a un estado final, no se buscan mas movidas
  final_state(Estado),
  % Se le da reversa al orden de la ruta para las Movidas
  reverse(Ruta, Movidas).

/**
 * Si no se encuentra el estado final,
 * se buscan nuevos estados
 */
solve_best([punto(Estado, Ruta, _)|Frontera], Historial, RutaFinal) :-
  % Obtener movidas del mejor Estado
  findall(M, move(Estado, M), Movidas),
  % Calcular nuevos estados a partir de Movidas
  updates(Movidas, Ruta, Estado, Estados),
  % Verificar que sean legales
  legals(Estados, Estados2),
  % Escoger aquellos estados que no hayan sido visitados
  news(Estados2, Historial, Estados3),
  % Calcular los valores heuristicos de los nuevos estados
  evaluates(Estados3, Valores),
  % Insertar en orden los nuevos puntos en la frontera 
  inserts(Valores, Frontera, Frontera2),
  % Continuar resolviendo
  solve_best(Frontera2, [Estado|Historial], RutaFinal).

/**
 * updates(Movidas, Ruta, Estado, Estados)
 * Estados es la lista de posibles estados a partir de Estado
 * utilizando la lista de movidas Movidas
 * Ruta es la ruta de movidas que se llevan desde initial_state
 */
updates([M|Ms], Ruta, S, [(S1, [M|Ruta])|Ss]) :-
    % obtiene el estado al que se llega por una movida
    update(S, M, S1),       
     % procesa recursivamente las siguientes movidas 
    updates(Ms, Ruta, S, Ss). 
updates([],_,_,[]).

/*
 * legasls(Estados, Estados2)
 *  Obtiene los estados legales de Estados
 *  Maneja pares (Estado,Ruta).
 */

% El primer estado es legal, incluirlo en la nueva lista
legals([(S, P)|Estados],[(S,P)|Estados1]) :-
    legal(S),
    legals(Estados,Estados1).
% Primer estado ilegal, excluirlo de la nueva lista
legals([(S,_)|Estados], Estados1) :-
    not(legal(S)),
    legals(Estados,Estados1).    
legals([],[]).

/*
 * news(Estados, Historial, Estados1)
 * Obtiene aquellos Estados que no hayan sido visitados
 * Estos son almacenados en Estados1
 * Maneja pares (Estado,Ruta).
 */
% primer estado ya aparece en historial, excluirlo de la nueva lista
news([(S, _)|Estados], Historial, Estados1) :-
    member(S, Historial),
    news(Estados, Historial, Estados1).
% primer estado no aparece en historial, incluirlo en nueva lista
news([(S, P)|Estados], Historial, [(S, P)|Estados1]) :-
    not(member(S, Historial)),
    news(Estados, Historial, Estados1).
news([],_,[]).

/*
 * evaluates(States,Values)
 *  Calcula el valor heuristico para todos los valores
 *  La lista Estados consiste de pares (Estado,Ruta).
 *  La lista Valores consiste de estructuras punto(Estado,Ruta,Valor).
 */
evaluates([(S, P)|Estados], [punto(S,P,V)|Valores]) :-
    value(S, V),                  % calcula valor heurístico del estado S
    evaluates(Estados, Valores).  % procesa resto de estados
evaluates([],[]).

/*
 * inserts(Points,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar una lista de puntos (Points)
 *   en una frontera anterior (Frontier).
 *   Los puntos son insertados preservando el orden descendente en el
 *   valor heurístico.
 */
inserts([Punto|Puntos], Frontier, Frontier1) :-
    insertPoint(Punto, Frontier, Frontier0),  % inserta primer punto
    inserts(Puntos, Frontier0, Frontier1).    % recursivamente inserta los demás puntos
inserts([], Frontier, Frontier).


/*
 * insertPoint(Point,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar el punto Points en
 *   su posición correcta dentro de Frontier de acuerdo con el orden
 *   del valor heurístico.
 *
 */
insertPoint(Point, [], [Point]).

% nuevo punto es mejor que el primero de la frontera,
% va de primero en nueva frontera
insertPoint(Point, [Point1|Points], [Point1,Point|Points]) :-
    less_than(Point1, Point).

% nuevo punto es igual al primero de la frontera,
% nuevo punto se ignora y se deja la frontera sin cambios
insertPoint(Point, [Point1|Points], [Point|Points]) :-
    equals(Point, Point1).

% nuevo punto es peor que el primero de la frontera,
% el primero de la frontera de deja en el primer lugar y
% el nuevo punto se inserta recursivamente dentro del resto de la frontera
insertPoint(Point, [Point1|Points], [Point1|Points1]) :-
    less_than(Point, Point1),
    insertPoint(Point, Points, Points1).

% nuevo punto no es igual a primero de la frontera pero tiene
% el mismo valor heurístico, se pone al nuevo punto como primero
% en la nueva frontera
insertPoint(Point, [Point1|Points], [Point,Point1|Points]) :-
    same(Point, Point1).

/*
 * relaciones de comparación de puntos
 *
 * no se puede dar el caso de que dos puntos tengan el mismo estado
 * pero diferente valor
 */

% dos puntos son iguales si contienen el mismo estado y tienen el mismo valor;
% se ignoran las rutas: no importa cómo se haya llegado al mismo estado
equals(punto(S, _, V), punto(S, _, V)).

% un punto es menor que otro, si contienen diferentes estados y si el
% valor del primero es menor que el valor del segundo;
% las rutas son ignoradas
less_than(punto(S1, _, V1), punto(S2, _, V2)) :- S1 \= S2, V1 < V2.

% dos puntos tienen el mismo valor si contienen diferentes estados
% y si sus valores son iguales
same(punto(S1, _, V1), punto(S2, _, V2)) :- S1 \= S2, V1 = V2.

/*
 * Inicializa un problema y lo resuelve.
 *   Problema: nombre del problema.
 *   Movidas: movidas requeridas para resolver el problema.
 */
test_best_search(Problema,Movidas) :-
   initial_state(Problema,Estado),   % obtener un Estado inicial dado Problema
   value(Estado,Valor),             % calcula el valor heurístico del estado incial
   solve_best([punto(Estado,[],Valor)],[Estado],Movidas). % inicializa frontera e historial,

/**
 * Estado Inicial
 * bridges_torch(Antorcha, CapacidadPuente, TiempoActual, TiempoLimite, LadoIzquierdo, LadoDerecho)
 */
initial_state(bridges_torch1, bridges_torch(izq, 2, 0, 28, [
      persona(a, 1),
      persona(b, 2),
      persona(c, 5),
      persona(d, 10),
      persona(e, 15)
], [])).
initial_state(bridges_torch2, bridges_torch(izq, 3, 0, 21, [
      persona(a, 1),
      persona(b, 2),
      persona(c, 5),
      persona(d, 10),
      persona(e, 15)
], [])).
initial_state(bridges_torch3, bridges_torch(izq, 2, 0, 42, [
      persona(a, 1),
      persona(b, 2),
      persona(c, 5),
      persona(d, 10),
      persona(e, 15),
      persona(j, 20)
], [])).
initial_state(bridges_torch4, bridges_torch(izq, 3, 0, 30, [
      persona(a, 1),
      persona(b, 2),
      persona(c, 5),
      persona(d, 10),
      persona(e, 15),
      persona(j, 20)
], [])).

% Estado Final
% No hay personas a la izquierda y la antorcha se encuentra a la derecha
final_state(bridges_torch(der, _, _, _, [], _)).

% Si la antorcha se encuentra a la derecha, tomar cualquier persona de la derecha
move(bridges_torch(der, _, _, _, _, PersonasDerecha), PersonaQueViajara):-
      create_combination(1, PersonasDerecha, PersonaQueViajara).
% Si la antorcha se encuentra a la izquierda, tomar N personas de la izquierda
move(bridges_torch(izq, N, _, _, PersonasIzquierda, _), PersonasQueViajan):-
      deben_viajar(N, PersonasIzquierda, NViajeros),
      create_combination(NViajeros, PersonasIzquierda, PersonasQueViajan).

/**
 * Obtiene el numero de personas que deben viajar
 * N es el numero maximo de personas
 * Personas es la lista de personas que hay en el extremo
 * R el numero de personas que deben viajar
 */
deben_viajar(N, Personas, R):-
      length(Personas, PersonasLength),
      PersonasLength < N,
      R is PersonasLength.
deben_viajar(N, Personas, R):- 
      length(Personas, PersonasLength), 
      PersonasLength >= N, 
      R is N.

% Obtiene todas las posibles combinaciones de List
% con una longitud de Size 
create_combination(Size, List, Combination):-
      length(Combination, Size),
      mem1(Combination, List).

% Obtiene combinaciones para una Lista Y
mem1([], Y).
mem1([H|T], Y):- member(H, Y), rest(H, Y, New), mem1(T,New).

rest(A, L, R):- append(_, [A|R], L), !.

% Dado un estado se actualizan los demas estados
update(bridges_torch(AVieja, N, TActual, TLimite, PIzquierda1, PDerecha1),
PViajando, 
bridges_torch(ANueva, N, TNuevo, TLimite, PIzquierda2, PDerecha2)):-
      % Actualizar lugar de la antorcha
      update_antorcha(AVieja, ANueva),
      % Actualizar extremos del puente                       
      update_extremos(AVieja, PViajando, PIzquierda1, PDerecha1, PIzquierda2, PDerecha2),
      % Actualizar el tiempo actual
      update_tiempo(TActual, PViajando, TNuevo).

% Actualizar lugar de la antorcha
update_antorcha(izq, der).
update_antorcha(der, izq).

% Tomar personas de la izquierda y moverlas a la derecha
update_extremos(izq, PViajando, PIzq1, PDer1, PIzq2, PDer2):-
      take(PViajando, PIzq1, PIzq2),
      append(PViajando, PDer1, PDer2).
% Tomar personas de la derecha y moverlas a la izquierda
update_extremos(der, PViajando, PIzq1, PDer1, PIzq2, PDer2):-
      take(PViajando, PDer1, PDer2),
      append(PViajando, PIzq1, PIzq2).

% Elimina todos los elementos de la lista L
% que se encuentran en la lista S
take(S,L,R):- findall(Z, (member(Z, L), not(member(Z, S))), R).

% Actualiza el tiempo actual sumandole el viaje nuevo
update_tiempo(TiempoActual, PersonasViajando, TiempoNuevo):-
      % Obtener lista de los tiempos de cada persona
      create_list_of_times(PersonasViajando, ListaNum),
      % Obtener el maximo
      max(ListaNum, MaxT),
      TiempoNuevo is MaxT + TiempoActual.

% Crea una lista de tiempos a partir de las personas que estan viajando
create_list_of_times([], []).
create_list_of_times([persona(_, Tiempo)|Tail], [Tiempo|T]):-
      create_list_of_times(Tail, T).

% Obtiene el mayor numero de una lista
max(L, M):- 
      member(M,L), 
      findall(X, (member(X, L), X > M), NL),
      length(NL, 0).

% Estado es legal si no sobrepasa el tiempo limite
legal(bridges_torch(_, _, TiempoActual, TiempoLimite, _, _)):-
      not(ilegal(TiempoActual, TiempoLimite)).

% Verificar que el estado no sobrepase el tiempo limite
ilegal(TiempoActual, TiempoLimite):- TiempoActual > TiempoLimite.

/**
 * value(Estado, Valor)
 * Dado un estado calcula el valor de la movida realizada
 * Mayor valor significa mejor Estado
 * Siempre se mueve el mas rapido hacia la izquierda
 */
value(bridges_torch(izq, _, _, _, _, []), 0).
value(bridges_torch(izq, _, TActual, TMaximo, _, _), Valor) :-
      Valor is TMaximo - TActual.
/**
 * Para obtener la mejor primer movida posible,
 * se deben mover los dos mas rapidos.
 * Esto se da cuando el tiempo actual es igual
 * a la persona mas lenta de la derecha.
 */
value(bridges_torch(der, _, TActual, TMaximo, _, PDerecha), Valor) :-
      create_list_of_times(PDerecha, TiemposDer),
      max_list(TiemposDer, TActual),
      Valor is TMaximo - TActual, !.
/**
 * Cuando se mueve a la derecha siempre se busca 
 * darle prioridad a los mas lentos
 */
value(bridges_torch(der, _, _, _, _, PDerecha), Valor) :-
      sum_list(PDerecha, Valor).

/**
 * sum_list(Lista, Sumatoria).
 * Suma todos los valores de Lista
 */
sum_list([], 0).
sum_list([persona(_, Tiempo)|T], Resultado) :-
      sum_list(T, R),
      Resultado is Tiempo + R.
