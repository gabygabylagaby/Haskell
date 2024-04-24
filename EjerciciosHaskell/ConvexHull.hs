module Main where

type X = Int
type Y = Int

sample :: [(Int, Int)]
sample = [
    (1, 1), 
    (2, 5), 
    (3, 3), 
    (5, 3), 
    (3, 2), 
    (2, 2)]


findMinX :: [(X, Y)] -> (X, Y) -> (X, Y)
findMinX [] min = min
findMinX (x:xs) min | fst x < fst min = findMinX xs x
                    | otherwise = findMinX xs min


findMaxX :: [(X, Y)] -> (X, Y) -> (X, Y)
findMaxX [] max = max
findMaxX (x:xs) max | fst x > fst max = findMaxX xs x
                    | otherwise = findMaxX xs max

findMaxY :: [(X, Y)] -> (X, Y) -> (X, Y)
findMaxY [] max = max
findMaxY (x:xs) max | snd x > snd max = findMaxY xs x
                    | otherwise = findMaxY xs max
                
findMinY :: [(X, Y)] -> (X, Y) -> (X, Y)
findMinY [] min = min
findMinY (x:xs) min | snd x < snd min = findMinY xs x
                    | otherwise = findMinY xs min

findGroupUp :: [(X, Y)] -> (X, Y) -> (X,Y) -> (X,Y) -> (X,Y) ->[(X, Y)]
findGroupUp [] minX maxX maxY minY = []
findGroupUp (x:xs) minX maxX minY maxY | fst x > fst minX = x : findGroupUp xs minX maxX minY maxY -- Si el punto esta arriba de la recta que une los puntos con menor y mayor coordenada x, lo agrega
                                       | fst x < fst maxX = x : findGroupUp xs minX maxX minY maxY -- Si el punto esta abajo de la recta que une los puntos con menor y mayor coordenada x, lo agrega
                                       | snd x > snd minY = x : findGroupUp xs minX maxX minY maxY -- Si el punto esta a la derecha de la recta que une los puntos con menor y mayor coordenada y, lo agrega
                                       | snd x < snd maxY = x : findGroupUp xs minX maxX minY maxY -- Si el punto esta a la izquierda de la recta que une los puntos con menor y mayor coordenada y, lo agrega
                                       | otherwise = findGroupUp xs minX maxX minY maxY-- Si no, lo ignora

obtenerPuntos :: [(X, Y)] -> (X, Y) -> (X,Y) -> (X,Y) -> (X,Y) ->[(X, Y)]
obtenerPuntos [] minX maxX minY maxY = []
obtenerPuntos (p:ps) minX maxX minY maxY
    | p `elem` [minX, maxX, minY, maxY] = p : obtenerPuntos ps minX maxX minY maxY
    | otherwise = obtenerPuntos ps minX maxX minY maxY 


distance :: (X, Y) -> (X, Y) -> Double
distance (x1, y1) (x2, y2) = sqrt (fromIntegral ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)) -- calculo la distancia entre dos puntos

-- | Convex hull
perimeter :: [(X, Y)] -> Double
perimeter [] = 0
perimeter [_] = 0
perimeter points = let pairs = zip points (tail points ++ [head points]) -- Crea pares de puntos consecutivos
    in sum $ map (\(p1, p2) -> distance p1 p2) pairs -- Calcula la distancia entre los puntos consecutivos


obtenerPuntosSecundarios :: [(X, Y)] -> (X, Y) -> (X, Y) -> (X, Y) -> (X, Y) -> [(X, Y)]
obtenerPuntosSecundarios [] minX maxX minY maxY = []
obtenerPuntosSecundarios (p:ps) minX maxX minY maxY
    | p `elem` [minX, maxX, minY, maxY] = obtenerPuntosSecundarios ps minX maxX minY maxY -- Ignorar puntos principales
    | fst p < fst minX || fst p > fst maxX || snd p < snd minY || snd p > snd maxY = p : obtenerPuntosSecundarios ps minX maxX minY maxY -- Considerar como punto importante si está fuera de los límites
    | otherwise = obtenerPuntosSecundarios ps minX maxX minY maxY -- Ignorar puntos dentro de los límites

compararListasPuntos :: [(X, Y)] -> [(X, Y)] -> Bool
compararListasPuntos xs ys = xs == ys

agregarListas :: [(X, Y)] -> [(X, Y)] -> [(X, Y)]
agregarListas xs ys = xs ++ ys

-- Main function
main :: IO ()   
main = do
    let minX = findMinX sample (head sample) 
        maxX = findMaxX sample (head sample) 
        maxY = findMaxY sample (head sample)
        minY = findMinY sample (head sample)
        groupUp = obtenerPuntos sample minX maxX maxY minY
        convexHullPerimeter = perimeter groupUp
    let (minX, maxX, minY, maxY) = (findMinX sample (head sample), findMaxX sample (head sample), findMinY sample (head sample), findMaxY sample (head sample))
        groupUp = obtenerPuntos sample minX maxX minY maxY
        importantPoints = obtenerPuntos sample minX maxX minY maxY
        sonIguales = compararListasPuntos groupUp importantPoints
        combinedList = if not sonIguales then agregarListas groupUp importantPoints else []
        combinedPerimeter = perimeter combinedList
    putStrLn $ "Son las dos listas iguales?: " ++ show sonIguales
    putStrLn $ "Lista combinada: " ++ show combinedList
    putStrLn $ "Perímetro de la lista combinada: " ++ show combinedPerimeter
    putStrLn $ "Important points secundarios: " ++ show importantPoints
    putStrLn $ "Perimeter of the convex hull: " ++ show convexHullPerimeter 
    putStrLn $ "Group up points: " ++ show groupUp

