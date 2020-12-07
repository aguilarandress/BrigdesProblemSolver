/**
 * Andres Esteban Aguilar Moya - 2019156214
 * Lenguajes de Programacion - Tarea Programada 2
 */

/**
 * solve_hill_climb(Estado, Historia, Movidas)
 * Movidas es la secuencia de pasos para alcanzar
 * un estado final.
 * Historia contiene los estados visitados previamente
 */
solve_hill_climb(Estado, _, []) :- final_state(Estado).
solve_hill_climb(Estado, Historia, [Movida|Movidas]) :-
      % Generar una nueva movida en el orden heuristico
      hill_climb(Estado, Movida),
      % Calcula un nuevo estado usando Movida
      update(Estado, Movida, Estado2),
      % El estado debe ser legal
      legal(Estado2),
      % Debe ser la primera vez que se llega al estado
      not(member(Estado2, Historia)),
      % Continuar buscando soluciones
      solve_hill_climb(Estado2, [Estado2|Historia], Movidas).

/**
 * Dado un estado devuelve una movida
 * Genera todas las movidas y las evalua
 * Cada movida se utilizara en orden decreciente
 */
hill_climb(Estado, Movida) :-
      findall(M, move(Estado, M), Movidas),   % Encontrar todas las movidas posibles
      evaluate_and_order(Movidas, Estado, [], MVs), % Evaluar todas las movidas
      member((Movida, _), MVs). % Escoge movidas en orden de heuristico

/**
 * Se evaluan todas las movidas del estado actual
 * Las movidas con almacenadas en orden en OrderedMVs
 * MVs es el acumulador parcial de las movidas
 */
evaluate_and_order([Movida|Movidas], Estado, MVs, OrderedMVs) :-
      update(Estado, Movida, Estado2),          % Obtener nuevo estado usando movida
      value(Estado2, Valor),                    % Asignar valor a dicho estado
      insert_pair((Movida, Valor), MVs, MVs1),  % Insertar movida con valor
      evaluate_and_order(Movidas, Estado, MVs1, OrderedMVs).
% Caso base: No quedan movidas por evaluar
evaluate_and_order([],_,MVs,MVs).

/**
 * value(Estado, Valor)
 * Dado un estado calcula el valor de la movida realizada
 * Mayor valor significa mejor Estado
 * Siempre se mueve el mas rapido hacia la izquierda
 */
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
      
/**
 * Inserta un par nuevo en la lista de movidas
 * Una vez insertadas se ordenan por su evaluacion
 */
insert_pair(MV,[],[MV]).
insert_pair((M,V),[(M1,V1)|MVs],[(M,V),(M1,V1)|MVs]) :-
    V >= V1.
insert_pair((M,V),[(M1,V1)|MVs],[(M1,V1)|MVs1]) :-
    V < V1,insert_pair((M,V),MVs,MVs1).

/**
 * Inicializa el problema y lo resuelve
 * Problema: El nombre del problema
 * Movidas: Las movidas necesarias para resolver el problema
 */
test_hill_climb(Problema, Movidas) :-
      initial_state(Problema, Estado),
      solve_hill_climb(Estado, [Estado], Movidas).

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