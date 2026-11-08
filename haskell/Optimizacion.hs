module Optimizacion where

import Data.List (foldl')
import Tipos

--Modulo 3: Metricas, ordenamiento (Quicksort) y optimos sobre listas de rutas
--Una Ruta es una lista de vuelos consecutivos (type Ruta = [Vuelo] en Tipos)

--Suma de los precios de cada vuelo de la ruta (reduccion con foldl')
costoTotalRuta :: Ruta -> Precio
costoTotalRuta = foldl' (\acum v -> acum + precio v) 0

--Llegada del ultimo vuelo - salida del primero (incluye esperas en escala)
--Para una ruta vacia devuelve 0 (funcion total, no lanza error)
tiempoTotalViaje :: Ruta -> Minutos
tiempoTotalViaje []   = 0
tiempoTotalViaje ruta = llegada (last ruta) - salida (head ruta)


--QUICKSORT FUNCIONAL (sin sort / sortBy / sortOn de Data.List)

--Quicksort generico: ordena ascendentemente segun la clave que da la funcion.
--Los empates van a la derecha del pivote (>=), asi el orden es estable.
quicksortPor :: Ord b => (a -> b) -> [a] -> [a]
quicksortPor _ [] = []
quicksortPor clave (pivote : resto) =
    quicksortPor clave menores ++ [pivote] ++ quicksortPor clave mayores
  where
    kp       = clave pivote
    menores  = filter (\x -> clave x <  kp) resto
    mayores  = filter (\x -> clave x >= kp) resto

--Ordena las rutas de menor a mayor costo economico
quicksortPorCosto :: [Ruta] -> [Ruta]
quicksortPorCosto = quicksortPor costoTotalRuta

--Ordena las rutas de menor a mayor tiempo total de viaje
quicksortPorTiempo :: [Ruta] -> [Ruta]
quicksortPorTiempo = quicksortPor tiempoTotalViaje

--DETERMINACION DE OPTIMOS

--Minimo segun una clave, con foldl' sobre (mejor ruta, valor) acumulados.
--En empate conserva la primera ruta encontrada. Lista vacia -> Nothing.
mejorRutaPor :: (Ruta -> Int) -> [Ruta] -> Maybe (Ruta, Int)
mejorRutaPor _ [] = Nothing
mejorRutaPor clave (r : rs) = Just (foldl' paso (r, clave r) rs)
  where
    paso (mejor, vMejor) x =
        let vx = clave x
        in if vx < vMejor then (x, vx) else (mejor, vMejor)

--Ruta de costo minimo y su costo
mejorRutaCosto :: [Ruta] -> Maybe (Ruta, Precio)
mejorRutaCosto = mejorRutaPor costoTotalRuta

--Ruta de menor tiempo total y su duracion en minutos
mejorRutaTiempo :: [Ruta] -> Maybe (Ruta, Minutos)
mejorRutaTiempo = mejorRutaPor tiempoTotalViaje
