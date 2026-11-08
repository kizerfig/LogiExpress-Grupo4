module Main where

import Tipos
import Parser (cargarVuelos, vuelosSalenDe, vuelosLleganA)
import Rutas (buscarRutas)
import Optimizacion
    ( costoTotalRuta
    , tiempoTotalViaje
    , quicksortPorCosto
    , quicksortPorTiempo
    , mejorRutaCosto
    , mejorRutaTiempo
    )

import Control.Exception (catch, SomeException)
import Control.Monad (when)
import Data.Char (toUpper)
import Data.List (intercalate)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
    grafo <- inicializarGrafo
    bucleMenu grafo tmcPorDefecto

inicializarGrafo :: IO Grafo
inicializarGrafo = do
    grafo1 <- intentarCargar "vuelos.txt"
    if not (null grafo1)
        then do
            putStrLn $ "[OK] Base de datos cargada exitosamente: " ++ show (length grafo1) ++ " vuelos registrados."
            return grafo1
        else do
            grafo2 <- intentarCargar "../vuelos.txt"
            if not (null grafo2)
                then do
                    putStrLn $ "[OK] Base de datos cargada exitosamente: " ++ show (length grafo2) ++ " vuelos registrados."
                    return grafo2
                else do
                    putStrLn "[AVISO] No se pudo cargar vuelos.txt automaticamente. Use la Opcion 1 para cargarlo."
                    return []

intentarCargar :: FilePath -> IO Grafo
intentarCargar ruta = catch (cargarVuelos ruta) (\(_ :: SomeException) -> return [])

bucleMenu :: Grafo -> TMC -> IO ()
bucleMenu grafo tmc = do
    putStrLn ""
    putStrLn "===================================================================="
    putStrLn "            LOGIEXPRESS - SISTEMA ANALITICO DE VUELOS               "
    putStrLn "                      (Paradigma Funcional)                         "
    putStrLn "===================================================================="
    putStrLn $ " [Tiempo Minimo de Conexion (TMC) Global: " ++ show tmc ++ " min]"
    putStrLn "--------------------------------------------------------------------"
    putStrLn "  1. Cargar / Recargar base de datos (vuelos.txt)"
    putStrLn "  2. Configurar Tiempo Minimo de Conexion (TMC)"
    putStrLn "  3. Consultar vuelos que salen de una ciudad"
    putStrLn "  4. Consultar vuelos que llegan a una ciudad"
    putStrLn "  5. Buscar todas las rutas validas (Origen -> Destino)"
    putStrLn "  6. Determinar Ruta de Costo Economico Minimo"
    putStrLn "  7. Determinar Ruta de Menor Tiempo Total de Viaje"
    putStrLn "  8. Reporte ordenado de itinerarios (por Costo o por Tiempo)"
    putStrLn "  9. Salir del sistema"
    putStrLn "===================================================================="
    putStr "Seleccione una opcion [1-9]: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            (nuevoGrafo, _) <- opcionCargarBase grafo
            pausar
            bucleMenu nuevoGrafo tmc
        "2" -> do
            nuevoTmc <- opcionConfigurarTMC tmc
            pausar
            bucleMenu grafo nuevoTmc
        "3" -> do
            opcionVuelosSalen grafo
            pausar
            bucleMenu grafo tmc
        "4" -> do
            opcionVuelosLlegan grafo
            pausar
            bucleMenu grafo tmc
        "5" -> do
            opcionBuscarRutas grafo tmc
            pausar
            bucleMenu grafo tmc
        "6" -> do
            opcionMejorCosto grafo tmc
            pausar
            bucleMenu grafo tmc
        "7" -> do
            opcionMejorTiempo grafo tmc
            pausar
            bucleMenu grafo tmc
        "8" -> do
            opcionReporteOrdenado grafo tmc
            pausar
            bucleMenu grafo tmc
        "9" -> do
            putStrLn ""
            putStrLn "===================================================================="
            putStrLn " Gracias por utilizar LogiExpress. Finalizando sesion...           "
            putStrLn "===================================================================="
        _ -> do
            putStrLn "[ERROR] Opcion no valida. Por favor ingrese un numero del 1 al 9."
            pausar
            bucleMenu grafo tmc

opcionCargarBase :: Grafo -> IO (Grafo, ())
opcionCargarBase grafoActual = do
    putStrLn ""
    putStrLn "--- CARGA DE BASE DE DATOS DE VUELOS ---"
    putStr "Ingrese la ruta del archivo [Enter para usar vuelos.txt por defecto]: "
    hFlush stdout
    rutaEntrada <- getLine
    let ruta = if null rutaEntrada then "vuelos.txt" else rutaEntrada
    resultado <- catch (do
        g <- cargarVuelos ruta
        putStrLn $ "[OK] Base de datos cargada exitosamente: " ++ show (length g) ++ " vuelos registrados."
        return (g, ())
        ) (\(e :: SomeException) -> do
            putStrLn $ "[ERROR] No se pudo cargar el archivo: " ++ show e
            return (grafoActual, ())
        )
    return resultado

opcionConfigurarTMC :: TMC -> IO TMC
opcionConfigurarTMC tmcActual = do
    putStrLn ""
    putStrLn "--- CONFIGURACION DE TIEMPO MINIMO DE CONEXION (TMC) ---"
    putStrLn $ "TMC actual: " ++ show tmcActual ++ " minutos."
    putStr "Ingrese nuevo TMC en minutos (ej. 45): "
    hFlush stdout
    linea <- getLine
    case readMaybe linea of
        Just n | n >= 0 -> do
            putStrLn $ "[EXITO] TMC global actualizado a " ++ show n ++ " minutos."
            return n
        _ -> do
            putStrLn "[ERROR] Valor invalido. Debe suministrar un numero entero no negativo."
            return tmcActual

opcionVuelosSalen :: Grafo -> IO ()
opcionVuelosSalen grafo = do
    putStrLn ""
    putStrLn "--- VUELOS SALIENTES POR CIUDAD ---"
    putStr "Ingrese nombre de la ciudad de origen (ej. Maiquetia, Pto Ordaz): "
    hFlush stdout
    ciudad <- getLine
    if null ciudad
        then putStrLn "[AVISO] No ingreso una ciudad."
        else do
            let vuelos = vuelosSalenDe ciudad grafo
            mostrarTablaVuelos vuelos "salen de" ciudad

opcionVuelosLlegan :: Grafo -> IO ()
opcionVuelosLlegan grafo = do
    putStrLn ""
    putStrLn "--- VUELOS ENTRANTES POR CIUDAD ---"
    putStr "Ingrese nombre de la ciudad de destino (ej. Porlamar, Valencia): "
    hFlush stdout
    ciudad <- getLine
    if null ciudad
        then putStrLn "[AVISO] No ingreso una ciudad."
        else do
            let vuelos = vuelosLleganA ciudad grafo
            mostrarTablaVuelos vuelos "llegan a" ciudad

opcionBuscarRutas :: Grafo -> TMC -> IO ()
opcionBuscarRutas grafo tmc = do
    putStrLn ""
    putStrLn "--- BUSQUEDA AVANZADA DE RUTAS VALIDAS ---"
    params <- capturarParametros
    case params of
        Nothing -> return ()
        Just (orig, dest, dia, maxEsc) -> do
            let rutas = buscarRutas grafo orig dest dia maxEsc tmc
            putStrLn $ "\nSe encontraron " ++ show (length rutas) ++ " rutas validas de \""
                     ++ orig ++ "\" a \"" ++ dest ++ "\" para el dia " ++ show dia ++ ":"
            mostrarReporteRutas rutas

opcionMejorCosto :: Grafo -> TMC -> IO ()
opcionMejorCosto grafo tmc = do
    putStrLn ""
    putStrLn "--- DETERMINACION DE RUTA DE COSTO MINIMO ---"
    params <- capturarParametros
    case params of
        Nothing -> return ()
        Just (orig, dest, dia, maxEsc) -> do
            let rutas = buscarRutas grafo orig dest dia maxEsc tmc
            case mejorRutaCosto rutas of
                Nothing -> putStrLn "[AVISO] No se encontraron rutas validas con los criterios suministrados."
                Just (mejor, costo) -> do
                    putStrLn "--------------------------------------------------------------------"
                    putStrLn $ ">>> RUTA OPTIMA EN COSTO ECONOMICO (Total: $" ++ show costo ++ ") <<<"
                    putStrLn "--------------------------------------------------------------------"
                    mostrarDetalleRuta 1 mejor

opcionMejorTiempo :: Grafo -> TMC -> IO ()
opcionMejorTiempo grafo tmc = do
    putStrLn ""
    putStrLn "--- DETERMINACION DE RUTA DE MENOR TIEMPO DE VIAJE ---"
    params <- capturarParametros
    case params of
        Nothing -> return ()
        Just (orig, dest, dia, maxEsc) -> do
            let rutas = buscarRutas grafo orig dest dia maxEsc tmc
            case mejorRutaTiempo rutas of
                Nothing -> putStrLn "[AVISO] No se encontraron rutas validas con los criterios suministrados."
                Just (mejor, tiempo) -> do
                    let hh = tiempo `div` 60
                        mm = tiempo `mod` 60
                    putStrLn "--------------------------------------------------------------------"
                    putStrLn $ ">>> RUTA OPTIMA EN TIEMPO (Duracion: " ++ show tiempo ++ " min [" ++ show hh ++ "h " ++ show mm ++ "m]) <<<"
                    putStrLn "--------------------------------------------------------------------"
                    mostrarDetalleRuta 1 mejor

opcionReporteOrdenado :: Grafo -> TMC -> IO ()
opcionReporteOrdenado grafo tmc = do
    putStrLn ""
    putStrLn "--- REPORTE ORDENADO DE ITINERARIOS ---"
    params <- capturarParametros
    case params of
        Nothing -> return ()
        Just (orig, dest, dia, maxEsc) -> do
            let rutas = buscarRutas grafo orig dest dia maxEsc tmc
            if null rutas
                then putStrLn "[AVISO] No se encontraron rutas validas."
                else do
                    putStrLn "Seleccione el criterio de ordenamiento:"
                    putStrLn "  1. Ordenar por Costo Economico (Ascendente)"
                    putStrLn "  2. Ordenar por Tiempo Total de Viaje (Ascendente)"
                    putStr "Criterio [1-2]: "
                    hFlush stdout
                    crit <- getLine
                    case crit of
                        "1" -> do
                            let ordenadas = quicksortPorCosto rutas
                            putStrLn ">>> ITINERARIOS ORDENADOS POR MENOR COSTO: <<<"
                            mostrarReporteRutas ordenadas
                        "2" -> do
                            let ordenadas = quicksortPorTiempo rutas
                            putStrLn ">>> ITINERARIOS ORDENADOS POR MENOR TIEMPO TOTAL: <<<"
                            mostrarReporteRutas ordenadas
                        _ -> putStrLn "[ERROR] Criterio invalido."

capturarParametros :: IO (Maybe (Ciudad, Ciudad, Dia, Maybe Int))
capturarParametros = do
    putStr "Ciudad de Origen: "
    hFlush stdout
    orig <- getLine
    putStr "Ciudad de Destino: "
    hFlush stdout
    dest <- getLine
    putStr "Dia de la semana (ej. Lunes, Martes o L, M, Mi, J, V, S, D): "
    hFlush stdout
    diaStr <- getLine
    case parsearDiaUsuario diaStr of
        Nothing -> do
            putStrLn "[ERROR] Dia invalido. Use L, M, Mi, J, V, S, D o el nombre completo."
            return Nothing
        Just dia -> do
            putStr "Maximo numero de escalas [Presione Enter para sin limite]: "
            hFlush stdout
            escStr <- getLine
            let maxEsc = if null escStr then Nothing else readMaybe escStr
            return (Just (orig, dest, dia, maxEsc))

parsearDiaUsuario :: String -> Maybe Dia
parsearDiaUsuario s = case map toUpper s of
    "L"         -> Just L
    "LUNES"     -> Just L
    "M"         -> Just M
    "MARTES"    -> Just M
    "MI"        -> Just Mi
    "MIERCOLES" -> Just Mi
    "MIÉRCOLES" -> Just Mi
    "J"         -> Just J
    "JUEVES"    -> Just J
    "V"         -> Just V
    "VIERNES"   -> Just V
    "S"         -> Just S
    "SABADO"    -> Just S
    "SÁBADO"    -> Just S
    "D"         -> Just D
    "DOMINGO"   -> Just D
    "DIARIO"    -> Just Diario
    _           -> Nothing

pausar :: IO ()
pausar = do
    putStrLn ""
    putStr "Presione Enter para continuar..."
    hFlush stdout
    _ <- getLine
    return ()

mostrarTablaVuelos :: [Vuelo] -> String -> Ciudad -> IO ()
mostrarTablaVuelos [] verbo ciudad =
    putStrLn $ "\nNo existen vuelos registrados que " ++ verbo ++ " \"" ++ ciudad ++ "\"."
mostrarTablaVuelos vuelos verbo ciudad = do
    putStrLn $ "\nTotal de vuelos que " ++ verbo ++ " \"" ++ ciudad ++ "\": " ++ show (length vuelos)
    putStrLn "----------------------------------------------------------------------------------"
    putStrLn " Codigo |      Origen      |     Destino      | Salida | Llegada |  Precio | Dias"
    putStrLn "----------------------------------------------------------------------------------"
    mapM_ imprimirFilaVuelo vuelos
    putStrLn "----------------------------------------------------------------------------------"

imprimirFilaVuelo :: Vuelo -> IO ()
imprimirFilaVuelo v = do
    let codStr  = padRight 6 (codigoVuelo v)
        origStr = padRight 16 (origen v)
        destStr = padRight 16 (destino v)
        salStr  = padRight 6 (minutosAHora (salida v))
        llegStr = padRight 7 (minutosAHora (llegada v))
        precStr = padRight 6 (show (precio v))
        diasStr = intercalate "-" (map show (dias v))
    putStrLn $ " " ++ codStr ++ " | " ++ origStr ++ " | " ++ destStr ++ " | "
             ++ salStr ++ " | " ++ llegStr ++ " | $" ++ precStr ++ " | " ++ diasStr

padRight :: Int -> String -> String
padRight n s = take n (s ++ repeat ' ')

mostrarReporteRutas :: [Ruta] -> IO ()
mostrarReporteRutas [] = putStrLn "  (No se encontro ninguna ruta que cumpla los criterios)"
mostrarReporteRutas rutas = mapM_ (uncurry mostrarDetalleRuta) (zip [1..] rutas)

mostrarDetalleRuta :: Int -> Ruta -> IO ()
mostrarDetalleRuta num ruta = do
    let costo = costoTotalRuta ruta
        dur = tiempoTotalViaje ruta
        hh = dur `div` 60
        mm = dur `mod` 60
        escalas = length ruta - 1
    putStrLn $ "\n[Ruta #" ++ show num ++ "] Escalas: " ++ show escalas
             ++ " | Costo Total: $" ++ show costo
             ++ " | Tiempo Total: " ++ show dur ++ " min (" ++ show hh ++ "h " ++ show mm ++ "m)"
    putStrLn "  Segmentos del Itinerario:"
    imprimirSegmentos ruta 1
    putStrLn "--------------------------------------------------------------------"

imprimirSegmentos :: [Vuelo] -> Int -> IO ()
imprimirSegmentos [] _ = return ()
imprimirSegmentos [v] i = do
    let dur = llegada v - salida v
    putStrLn $ "    (" ++ show i ++ ") [" ++ codigoVuelo v ++ "] "
             ++ origen v ++ " -> " ++ destino v ++ " | "
             ++ minutosAHora (salida v) ++ " - " ++ minutosAHora (llegada v)
             ++ " (" ++ show dur ++ " min) | $" ++ show (precio v)
imprimirSegmentos (v1 : v2 : resto) i = do
    let dur = llegada v1 - salida v1
        espera = salida v2 - llegada v1
    putStrLn $ "    (" ++ show i ++ ") [" ++ codigoVuelo v1 ++ "] "
             ++ origen v1 ++ " -> " ++ destino v1 ++ " | "
             ++ minutosAHora (salida v1) ++ " - " ++ minutosAHora (llegada v1)
             ++ " (" ++ show dur ++ " min) | $" ++ show (precio v1)
    putStrLn $ "        * Conexion en " ++ destino v1 ++ ": espera de " ++ show espera ++ " minutos."
    imprimirSegmentos (v2 : resto) (i + 1)
