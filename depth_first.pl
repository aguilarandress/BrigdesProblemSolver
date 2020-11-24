/**
* Lenguajes de Programacion - Tarea Programada 2
* Andres Esteban Aguilar Moya - 2019156214
*/

% Si se llega al estado final, se termina la solucion
solve_bridges_depth_first(Estado, _, []) :- final_state(Estado).
% Resolver con Depth First
solve_bridges_depth_first(Estado,Historia,[Movida|Movidas]) :-
      move(Estado, Movida),          % Dado un estado se genera una movida
      update(Estado,Movida,Estado2), % Actualizar el estado con la nueva movida
      legal(Estado2),                % Verificar si el estado es legal
      not(member(Estado2,Historia)), % Verificar que dicha estado no haya sido visitado
      solve_bridges_depth_first(Estado2,[Estado2|Historia],Movidas).

/*    
 * Inicializa un problema y lo resuelve.
 *   Problema: nombre del problema.
 *   Movidas: movidas requeridas para resolver el problema.
 */
test_solution(Problema,Movidas) :-
      initial_state(Problema,Estado),                      % Obtener un Estado inicial dado Problema
      solve_bridges_depth_first(Estado,[Estado],Movidas).  % Inicia resolucion desde estado

/**
 * Estado Inicial
 * bridges_torch(Antorcha, CapacidadPuente, TiempoActual, TiempoLimite, LadoIzquierdo, LadoDerecho)
 */
initial_state(bridges_torch, bridges_torch(izq, 2, 0, 42, [
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
update_extremos(izq, PersonasViajando, PersonasIzquierda1, PersonasDerecha1, PersonasIzquierda2, PersonasDerecha2):-
      take(PersonasViajando, PersonasIzquierda1, PersonasIzquierda2),
      append(PersonasViajando, PersonasDerecha1, PersonasDerecha2).
% Tomar personas de la derecha y moverlas a la izquierda
update_extremos(der, PersonasViajando, PersonasIzquierda1, PersonasDerecha1, PersonasIzquierda2, PersonasDerecha2):-
      take(PersonasViajando, PersonasDerecha1, PersonasDerecha2),
      append(PersonasViajando, PersonasIzquierda1, PersonasIzquierda2).

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




