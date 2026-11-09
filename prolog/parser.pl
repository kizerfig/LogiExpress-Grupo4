% hechos dinamicos
:- dynamic vuelo/7.
:- dynamic tmc_config/1.

% tmc por defecto 45 min
tmc_config(45).

% obtiene el tmc actual
obtener_tmc(Minutos) :-
    tmc_config(Minutos), !.
obtener_tmc(45).

% cambia el tmc
establecer_tmc(Minutos) :-
    integer(Minutos),
    Minutos >= 0,
    retractall(tmc_config(_)),
    assertz(tmc_config(Minutos)).

% convierte hora militar a minutos
hora_a_minutos(HoraStr, MinutosTotales) :-
    atom_string(HoraAtom, HoraStr),
    atomic_list_concat([HHStr, MMStr], ':', HoraAtom),
    atom_number(HHStr, HH),
    atom_number(MMStr, MM),
    HH >= 0, HH =< 23,
    MM >= 0, MM =< 59,
    MinutosTotales is (HH * 60) + MM.

% convierte minutos a formato militar
minutos_a_hora(MinutosTotales, HoraStr) :-
    HH is MinutosTotales // 60,
    MM is MinutosTotales mod 60,
    format(atom(HoraStr), '~|~`0t~d~2+:~|~`0t~d~2+', [HH, MM]).

% busca vuelos.txt en la ruta por defecto
cargar_vuelos_defecto :-
    (   exists_file('../vuelos.txt')
    ->  cargar_vuelos('../vuelos.txt')
    ;   exists_file('vuelos.txt')
    ->  cargar_vuelos('vuelos.txt')
    ;   throw(error(archivo_no_encontrado, 'No se encontro el archivo vuelos.txt'))
    ).

% lee el archivo y guarda los hechos en memoria
cargar_vuelos(RutaArchivo) :-
    retractall(vuelo(_, _, _, _, _, _, _)),
    open(RutaArchivo, read, Stream, [encoding(utf8)]),
    leer_lineas(Stream, 0, TotalCargados),
    close(Stream),
    format('~N[OK] Base de datos cargada exitosamente: ~w vuelos registrados.~n', [TotalCargados]).

% lee linea por linea hasta el final
leer_lineas(Stream, Acc, Total) :-
    read_line_to_string(Stream, Linea),
    (   Linea == end_of_file
    ->  Total = Acc
    ;   normalize_space(string(Limpia), Linea),
        (   Limpia = ""
        ->  leer_lineas(Stream, Acc, Total)
        ;   procesar_linea(Limpia),
            NuevoAcc is Acc + 1,
            leer_lineas(Stream, NuevoAcc, Total)
        )
    ).

% separa los campos y aserta el vuelo
procesar_linea(LineaString) :-
    split_string(LineaString, "|", " \t\r\n", Partes),
    Partes = [OrigStr, DestStr, SalidaStr, LlegadaStr, PrecioStr, IdStr, DiasStr],
    atom_string(Origen, OrigStr),
    atom_string(Destino, DestStr),
    hora_a_minutos(SalidaStr, SalidaMin),
    hora_a_minutos(LlegadaStr, LlegadaMin),
    number_string(PrecioVal, PrecioStr),          % leer como entero
    (   PrecioVal < 0
    ->  format('~N[Error] Precio negativo encontrado en registro: ~w~n', [PrecioStr]),
        fail                                      % aborta la carga de esa línea
    ;   true
    ),
    atom_string(IDVuelo, IdStr),
    parsear_dias(DiasStr, ListaDias),
    assertz(vuelo(Origen, Destino, SalidaMin, LlegadaMin,
                  PrecioVal, IDVuelo, ListaDias)).

% pasa los dias a lista de atomos
parsear_dias("Diario", ['Diario']) :- !.
parsear_dias(DiasStr, ListaDias) :-
    split_string(DiasStr, "-", " \t\r\n", Tokens),
    maplist(atom_string, ListaDias, Tokens).

% busca vuelos que salen de una ciudad
vuelos_salen_de(Ciudad, ListaVuelos) :-
    findall(
        vuelo(Ciudad, Destino, Salida, Llegada, Precio, ID, Dias),
        vuelo(Ciudad, Destino, Salida, Llegada, Precio, ID, Dias),
        ListaVuelos
    ).

% busca vuelos que llegan a una ciudad
vuelos_llegan_a(Ciudad, ListaVuelos) :-
    findall(
        vuelo(Origen, Ciudad, Salida, Llegada, Precio, ID, Dias),
        vuelo(Origen, Ciudad, Salida, Llegada, Precio, ID, Dias),
        ListaVuelos
    ).
