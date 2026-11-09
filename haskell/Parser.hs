module Parser where

import Tipos
import System.IO

splitOn :: Char -> String -> [String]
splitOn delim str = case break (== delim) str of
    (elem, [])      -> [elem]
    (elem, _:resto) -> elem : splitOn delim resto

parsearDia :: String -> Dia
parsearDia "L"      = L
parsearDia "M"      = M
parsearDia "Mi"     = Mi
parsearDia "J"      = J
parsearDia "V"      = V
parsearDia "S"      = S
parsearDia "D"      = D
parsearDia "Diario" = Diario
parsearDia d        = error ("Dia desconocido: " ++ d)

parsearDias :: String -> [Dia]
parsearDias "Diario" = [Diario]
parsearDias str      = map parsearDia (splitOn '-' str)

parsearLinea :: String -> Vuelo
parsearLinea linea = case splitOn '|' linea of
    [orig, dest, salStr, llegStr, precStr, idV, diasStr] ->
        let precioVal = read precStr :: Int
            salMin    = horaAMinutos salStr
            llegMin   = horaAMinutos llegStr
        in if precioVal < 0
            then error $ "Precio negativo en registro: " ++ precStr
            else if orig == dest
                then error $ "Origen y destino no pueden ser iguales en registro: " ++ idV
                else if salMin >= llegMin
                    then error $ "Hora de llegada debe ser posterior a salida en registro: " ++ idV
                    else Vuelo
                        { origen      = orig
                        , destino     = dest
                        , salida      = salMin
                        , llegada     = llegMin
                        , precio      = precioVal
                        , codigoVuelo = idV
                        , dias        = parsearDias diasStr
                        }
    _ -> error "Formato de linea invalido"

cargarVuelos :: FilePath -> IO Grafo
cargarVuelos ruta = do
    contenido <- readFile ruta
    let lineas = filter (not . null) (lines contenido)
    return (map parsearLinea lineas)

vuelosSalenDe :: Ciudad -> Grafo -> [Vuelo]
vuelosSalenDe c grafo = filter (\v -> origen v == c) grafo

vuelosLleganA :: Ciudad -> Grafo -> [Vuelo]
vuelosLleganA c grafo = filter (\v -> destino v == c) grafo
