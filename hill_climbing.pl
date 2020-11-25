/**
 * Andres Esteban Aguilar Moya - 2019156214
 * Lenguajes de Programacion - Tarea Programada 2
 */


/**
 * Estado Inicial
 * bridges_torch(Antorcha, CapacidadPuente, TiempoActual, TiempoLimite, LadoIzquierdo, LadoDerecho)
 */
initial_state(bridges_torch, bridges_torch(izq, 3, 0, 21, [
      persona(a, 1),
      persona(b, 2),
      persona(c, 5),
      persona(d, 10),
      persona(e, 15)
], [])).

% Estado Final
% No hay personas a la izquierda y la antorcha se encuentra a la derecha
final_state(bridges_torch(der, _, _, _, [], _)).