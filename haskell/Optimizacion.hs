module Optimizacion where

import Data.List (foldl')
import Tipos

-- suma el costo de los vuelos de la ruta
costoTotalRuta :: Ruta -> Precio
costoTotalRuta = foldl' (\acum v -> acum + precio v) 0

-- calcula el tiempo total entre salida inicial y llegada final
tiempoTotalViaje :: Ruta -> Minutos
tiempoTotalViaje []   = 0
tiempoTotalViaje ruta = llegada (last ruta) - salida (head ruta)

-- quicksort generico
quicksortPor :: Ord b => (a -> b) -> [a] -> [a]
quicksortPor _ [] = []
quicksortPor clave (pivote : resto) =
    quicksortPor clave menores ++ [pivote] ++ quicksortPor clave mayores
  where
    kp       = clave pivote
    menores  = filter (\x -> clave x <  kp) resto
    mayores  = filter (\x -> clave x >= kp) resto

-- ordena las rutas por costo
quicksortPorCosto :: [Ruta] -> [Ruta]
quicksortPorCosto = quicksortPor costoTotalRuta

-- ordena las rutas por tiempo
quicksortPorTiempo :: [Ruta] -> [Ruta]
quicksortPorTiempo = quicksortPor tiempoTotalViaje

-- busca el minimo segun una funcion
mejorRutaPor :: (Ruta -> Int) -> [Ruta] -> Maybe (Ruta, Int)
mejorRutaPor _ [] = Nothing
mejorRutaPor clave (r : rs) = Just (foldl' paso (r, clave r) rs)
  where
    paso (mejor, vMejor) x =
        let vx = clave x
        in if vx < vMejor then (x, vx) else (mejor, vMejor)

-- ruta mas barata
mejorRutaCosto :: [Ruta] -> Maybe (Ruta, Precio)
mejorRutaCosto = mejorRutaPor costoTotalRuta

-- ruta mas rapida
mejorRutaTiempo :: [Ruta] -> Maybe (Ruta, Minutos)
mejorRutaTiempo = mejorRutaPor tiempoTotalViaje
