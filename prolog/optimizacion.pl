% modulo 3: metricas, ordenamiento y optimos

:- ensure_loaded('parser.pl').
:- catch(ensure_loaded('rutas.pl'), _, true).

% suma el costo de los vuelos de la ruta
costo_total_ruta(Ruta, CostoTotal) :-
    costo_acum(Ruta, 0, CostoTotal).

costo_acum([], Acum, Acum).
costo_acum([vuelo(_, _, _, _, Precio, _, _) | Resto], Acum, Costo) :-
    Acum1 is Acum + Precio,
    costo_acum(Resto, Acum1, Costo).

% calcula el tiempo total restando salida inicial de llegada final
tiempo_total_viaje(Ruta, DuracionMinutos) :-
    Ruta = [vuelo(_, _, Salida, _, _, _, _) | _],
    ultimo_vuelo(Ruta, vuelo(_, _, _, Llegada, _, _, _)),
    DuracionMinutos is Llegada - Salida.

ultimo_vuelo([X], X) :- !.
ultimo_vuelo([_ | Resto], U) :- ultimo_vuelo(Resto, U).

% quicksort por costo
quicksort_por_costo(ListaRutas, RutasOrdenadas) :-
    quicksort(costo_total_ruta, ListaRutas, RutasOrdenadas).

% quicksort por tiempo
quicksort_por_tiempo(ListaRutas, RutasOrdenadas) :-
    quicksort(tiempo_total_viaje, ListaRutas, RutasOrdenadas).

% quicksort generico
quicksort(_, [], []).
quicksort(Criterio, [Pivote | Resto], Ordenada) :-
    call(Criterio, Pivote, VP),
    particion(Criterio, Resto, VP, Menores, Mayores),
    quicksort(Criterio, Menores, OrdMenores),
    quicksort(Criterio, Mayores, OrdMayores),
    concatenar(OrdMenores, [Pivote | OrdMayores], Ordenada).

% divide la lista comparando con el pivote
particion(_, [], _, [], []).
particion(Criterio, [X | Xs], VP, [X | Menores], Mayores) :-
    call(Criterio, X, VX),
    VX < VP, !,
    particion(Criterio, Xs, VP, Menores, Mayores).
particion(Criterio, [X | Xs], VP, Menores, [X | Mayores]) :-
    particion(Criterio, Xs, VP, Menores, Mayores).

concatenar([], L, L).
concatenar([X | Xs], L, [X | R]) :- concatenar(Xs, L, R).

% busca la ruta con menor costo
mejor_ruta_costo(ListaRutas, MejorRuta, CostoMinimo) :-
    mejor_ruta(costo_total_ruta, ListaRutas, MejorRuta, CostoMinimo).

% busca la ruta con menor tiempo
mejor_ruta_tiempo(ListaRutas, MejorRuta, TiempoMinimo) :-
    mejor_ruta(tiempo_total_viaje, ListaRutas, MejorRuta, TiempoMinimo).

% compara las rutas para encontrar el minimo
mejor_ruta(Criterio, [R | Rs], Mejor, Valor) :-
    call(Criterio, R, V),
    mejor_aux(Criterio, Rs, R, V, Mejor, Valor).

mejor_aux(_, [], Mejor, Valor, Mejor, Valor).
mejor_aux(Criterio, [R | Rs], MejorAct, ValAct, Mejor, Valor) :-
    call(Criterio, R, V),
    (   V < ValAct
    ->  mejor_aux(Criterio, Rs, R, V, Mejor, Valor)
    ;   mejor_aux(Criterio, Rs, MejorAct, ValAct, Mejor, Valor)
    ).
