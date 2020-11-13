/*
 * solve_hill_climb(State,History,Moves)
 *   Moves es la secuencia de movidas requeridas para
 *   alcanzar un estado final deseado a partir de State.
 *   History contiene los estados previamente visitados.
 */

% Si el Estado actual es un estado final, no hay que moverse.
solve_hill_climb(State,_,[]) :-
    final_state(State).

/*
 * Si el Estado actual no es un estado final, genera una movida
 * para desplazarse a un nuevo estado, y continua la búsqueda a
 * partir de ese nuevo estado.
 * Las movidas son intentadas en el orden establecido por la heurística
 * que evalúa la "bondad" de los estados que se alcanzan para cada movida.
 */
solve_hill_climb(State,History,[Move|Moves]) :-
    hill_climb(State,Move),      % generar una nueva Movida en el orden heurístico
    update(State,Move,State1),   % calcula nuevo estado usando Movida
    legal(State1),               % nuevo estado debe ser legal
    not(member(State1,History)), % debe ser primera vez que se llega al nuevo estado
    solve_hill_climb(State1,[State1|History],Moves).   % continuar a partir de nuevo estado

/*
 *  A partir de un Estado devuelve una Movida.
 *  Primero genera todas las movidas, luego las evalúa usando una heurística,
 *  y finalmente las va usando en orden decreciente de la evaluación obtenida.
 */
hill_climb(State,Move) :-
    findall(M,move(State,M),Moves),         % Encuentra todas las movidas posibles
    evaluate_and_order(Moves,State,[],MVs), % Evalúa con la heurística todas las movidas y las ordena.
    member((Move,_),MVs).                   % Escoge movidas en orden de heurística


/*
 * evaluate_and_order(Movidas,Estado,AcumuladorParcial,MovidasOrdenadas)
 *   Todas las Movidas del Estado actual
 *   son evaluadas y almacenadas en orden en MovidasOrdenadas
 *   Acumulador es donde se van acumulando parcialmente las movidas evaluadas.
 */

% Caso: procesar la primera movida y continuar recursivamente
evaluate_and_order([Move|Moves],State,MVs,OrderedMVs) :-
    update(State,Move,State1),         % obtiene nuevo estado usando movida
    value(State1,Value),               % calcula el valor heurísico del nuevo estado
    insertPair((Move,Value),MVs,MVs1), % inserta en orden el par (movida,valor) en lista de movidas
    evaluate_and_order(Moves,State,MVs1,OrderedMVs).  % procesa recursivamente el resto de movidas
    
% Caso base: no hay más movidas que evaluar. Se retorna el acumulador como resultado.
evaluate_and_order([],_,MVs,MVs).

insertPair(MV,[],[MV]).
insertPair((M,V),[(M1,V1)|MVs],[(M,V),(M1,V1)|MVs]) :-
    V >= V1.
insertPair((M,V),[(M1,V1)|MVs],[(M1,V1)|MVs1]) :-
    V < V1,insertPair((M,V),MVs,MVs1).


/*
 * Inicializa un problema y lo resuelve.
 *   Problem: nombre del problema.
 *   Moves: movidas requeridas para resolver el problema.
 */
test_hill_climb(Problem,Moves) :-
   initial_state(Problem,State),           % obtener un Estado inicial dado Problema
   solve_hill_climb(State,[State],Moves).  % inicia resolución desde Estado



% === Relaciones que definen el problema zgm     === %
% === Son las mismas incluidas en depth-first.pl === %

initial_state(zgm,zgm(izq,[zorra,gallina,maiz],[])).

final_state(zgm(der,[],[zorra,gallina,maiz])).

move(zgm(izq,I,_),Carga):-member(Carga,I).
move(zgm(der,_,D),Carga):-member(Carga,D).
move(zgm(_,_,_),solo).

update(zgm(B,I,D),Carga,zgm(B1,I1,D1)):-
update_Bote(B,B1),
update_margenes(Carga,B,I,D,I1,D1).

update_Bote(izq,der).
update_Bote(der,izq).

update_margenes(solo,_,I,D,I,D).
update_margenes(Carga,izq,I,D,I1,D1):-
      select(Carga,I,I1),
      insert(Carga,D,D1).
update_margenes(Carga,der,I,D,I1,D1):-
      select(Carga,D,D1),
      insert(Carga,I,I1).

insert(X,[Y|Ys],[X,Y|Ys]):-precedes(X,Y).
insert(X,[Y|Ys],[Y|Zs]):-precedes(Y,X),insert(X,Ys,Zs).
insert(X,[],[X]).

select(X,[X|Xs],Xs).
select(X,[Y|Ys],[Y|Zs]):-select(X,Ys,Zs).

% Caso no determinístico
% precedes(zorra,_).
% precedes(_,maiz).

% Caso determinístico
precedes(zorra,gallina).
precedes(zorra,maiz).
precedes(gallina,maiz).


legal(zgm(izq,_,D)):-not(ilegal(D)).
legal(zgm(der,I,_)):-not(ilegal(I)).

ilegal(L):-member(zorra,L),member(gallina,L).
ilegal(L):-member(gallina,L),member(maiz,L).

% === Fin de las relaciones usadas por depth-first.pl ==%


% === Relación adicional requerida por hill-climb para resolver zgm. === %
% === Value/2 es una heurísica que da valores más altos conforme     === %
% === haya más cosas en la rivera derecha.                           === %

value(zgm(_,_,[]),0).
value(zgm(_,_,[_]),1).
value(zgm(_,_,[_,_]),2).
value(zgm(_,_,[_,_,_]),3).


