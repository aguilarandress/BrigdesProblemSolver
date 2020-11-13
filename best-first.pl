/*
 * solve_best/3 usa búsqueda por anchura
 *   El primer elemento es una lista de puntos que deben ser explorados.
 *   Esta lista es conocida como la frontera (Frontier).
 *
 *   El segundo elemento de solve_best/3 es el historial de los puntos que
 *   que ya han sido explorados y que no deben ser explorados de nuevo.
 *
 *   El tercer elemento de solve_best/3 la secuencia de movidas requeridas para
 *   alcanzar un estado final deseado a partir de Point.
 *
 *   Cada punto de exploración en la frontera tiene la forma
 *       state(Estado, Ruta, Valor)
 *   Dónde
 *       Estado es la descripción del estado en que se encuentra la
 *              resolución del problema independientemente del mecanismo
 *              de búsqueda. Esto es, es el mismo tipo de estado usado en
 *              búsquedas como depth-first o hill-climbing.
 *       Ruta es la secuencia de movidas que se requieren para llegar del
 *              estado incial a Estado.
 *       Valor es el estimado heurístico de cuán bueno es este estado para
 *              alcanzar el estado final.
 *   La lista de puntos se ecuentra ordenada en forma decreciente por Valor.
 */

 /*
  * Si el mejor punto en la frontera corresponde a un estado final,
  * no hay que buscar más.
  * Se obtiene la secuencia de movidas que llevan del estado inicial a este
  * estado final simplemente revirtiendo el orden de movidas encontradas en
  * la ruta correspondiente a este estado.
  */
solve_best([punto(State,Path,_)|_],_,Moves) :-
    final_state(State),reverse(Path,Moves).

/*
 * Si el mejor punto en la frontera no corresponde a un estado final:
 *     * se generan todas las movidas posibles a partir del estado de ese punto
 *     * se obtienen los nuevos estados que se alcanzarían con esas movidas
 *     * se calcularían los valores heurísticos de los nuevos estados
 *     * se introducen los nuevos estados como nuevo puntos en la frontera
 *     * se elimina el mejor punto de la frontera y se incluye en el historial.
 */
 
solve_best([punto(State,Path,_)|Frontier],History,FinalPath) :-
    findall(M,move(State,M),Moves),     % obtiene movidas del mejor estado
    updates(Moves,Path,State,States),   % obtiene los nuevos estados usando movidas
    legals(States,States1),             % escoge los nuevos estados que son legales
    news(States1,History,States2),      % elimina nuevos estados ya incluidos en historial
    evaluates(States2,Values),          % calcula valores heurísticos de los nuevos estados
    inserts(Values,Frontier,Frontier1), % inserta en orden los nuevos puntos en la frontera
    solve_best(Frontier1,[State|History],FinalPath). % continuar a partir de nueva frontera


/*
 * updates(Moves,Path,State,States)
 *   States es la lista de posibles estados accesables a partir
 *   de State usando la lista de posibles movidas (Moves).
 *   Path es la ruta de movidas que llevan del estado inicial a State.
 *   Las rutas de los nuevos estados se agregan al inicio su respectiva movida
 *   a la ruta de State.
 *   States es una lista de pares (NuevoEstado, NuevaRuta).
 */

updates([M|Ms],Path,S,[(S1,[M|Path])|Ss]) :-
    update(S,M,S1),         % obtiene el estado al que se llega por una movida
    updates(Ms,Path,S,Ss).  % procesa recursivamente las siguientes movidas
updates([],_,_,[]).


/*
 * legasls(States,States1)
 *   States1 es el subconjunto de la lista State que son estados legales.
 *   Maneja pares (Estado,Ruta).
 */

% el primer estado es legal, incluirlo en la nueva lista
legals([(S,P)|States],[(S,P)|States1]) :-
    legal(S),
    legals(States,States1).
    
% primer estado ilegal, excluirlo de la nueva lista
legals([(S,_)|States],States1) :-
    not(legal(S)),
    legals(States,States1).
    
legals([],[]).


/*
 * news(States,History,States1)
 *   States1 es el subconjunto de la lista States que consiste de estados
 *   que no aparecen en el historial.
 *   Maneja pares (Estado,Ruta).
 */

% primer estado ya aparece en historial, excluirlo de la nueva lista
news([(S,_)|States],History,States1) :-
    member(S,History),
    news(States,History,States1).

% primer estado no aparece en historial, incluirlo en nueva lista
news([(S,P)|States],History,[(S,P)|States1]) :-
    not(member(S,History)),
    news(States,History,States1).
    
news([],_,[]).


/*
 * evaluates(States,Values)
 *   Calcula el valor heurístico de los estados en la lista States.
 *   Values is la lista resultante con los estados junto con sus valores.
 *   La lista State consiste de pares (Estado,Ruta).
 *   La lista Values consiste de estructuras punto(Estado,Ruta,Valor).
 */

evaluates([(S,P)|States],[punto(S,P,V)|Values]) :-
    value(S,V),                % calcula valor heurístico del estado S
    evaluates(States,Values).  % procesa resto de estados
evaluates([],[]).


/*
 * inserts(Points,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar una lista de puntos (Points)
 *   en una frontera anterior (Frontier).
 *   Los puntos son insertados preservando el orden descendente en el
 *   valor heurístico.
 */

inserts([Punto|Puntos],Frontier,Frontier1) :-
    insertPoint(Punto,Frontier,Frontier0),  % inserta primer punto
    inserts(Puntos,Frontier0,Frontier1).    % recursivamente inserta los demás puntos
inserts([],Frontier,Frontier).


/*
 * insertPoint(Point,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar el punto Points en
 *   su posición correcta dentro de Frontier de acuerdo con el orden
 *   del valor heurístico.
 *
 */
insertPoint(Point,[],[Point]).

% nuevo punto es mejor que el primero de la frontera,
% va de primero en nueva frontera
insertPoint(Point,[Point1|Points],[Point1,Point|Points]) :-
    less_than(Point1,Point).

% nuevo punto es igual al primero de la frontera,
% nuevo punto se ignora y se deja la frontera sin cambios
insertPoint(Point,[Point1|Points],[Point|Points]) :-
    equals(Point,Point1).

% nuevo punto es peor que el primero de la frontera,
% el primero de la frontera de deja en el primer lugar y
% el nuevo punto se inserta recursivamente dentro del resto de la frontera
insertPoint(Point,[Point1|Points],[Point1|Points1]) :-
    less_than(Point,Point1),
    insertPoint(Point,Points,Points1).

% nuevo punto no es igual a primero de la frontera pero tiene
% el mismo valor heurístico, se pone al nuevo punto como primero
% en la nueva frontera
insertPoint(Point,[Point1|Points],[Point,Point1|Points]) :-
    same(Point,Point1).


/*
 * relaciones de comparación de puntos
 *
 * no se puede dar el caso de que dos puntos tengan el mismo estado
 * pero diferente valor
 */

% dos puntos son iguales si contienen el mismo estado y tienen el mismo valor;
% se ignoran las rutas: no importa cómo se haya llegado al mismo estado
equals(punto(S,_,V),punto(S,_,V)).

% un punto es menor que otro, si contienen diferentes estados y si el
% valor del primero es menor que el valor del segundo;
% las rutas son ignoradas
less_than(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 < V2.

% dos puntos tienen el mismo valor si contienen diferentes estados
% y si sus valores son iguales
same(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 = V2.



/*
 * Inicializa un problema y lo resuelve.
 *   Problem: nombre del problema.
 *   Moves: movidas requeridas para resolver el problema.
 */
test_best_search(Problem,Moves) :-
   initial_state(Problem,State),   % obtener un Estado inicial dado Problema
   value(State,Value),             % calcula el valor heurístico del estado incial
   solve_best([punto(State,[],Value)],[State],Moves). % inicializa frontera e historial,
                                                      % inicia resolución


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

%precedes(zorra,_).
%precedes(_,maiz).
precedes(zorra,gallina).
precedes(zorra,maiz).
precedes(gallina,maiz).


legal(zgm(izq,_,D)):-not(ilegal(D)).
legal(zgm(der,I,_)):-not(ilegal(I)).

ilegal(L):-member(zorra,L),member(gallina,L).
ilegal(L):-member(gallina,L),member(maiz,L).

% === Fin de las relaciones usadas por depth-first.pl ==%


% === Relación adicional requerida por best_first.pl para resolver zgm. === %
% === Value/2 es una heurísica que da valores más altos conforme     === %
% === haya más cosas en la rivera derecha.                           === %

value(zgm(_,_,[]),0).
value(zgm(_,_,[_]),1).
value(zgm(_,_,[_,_]),2).
value(zgm(_,_,[_,_,_]),3).


