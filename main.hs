module Main2 where

type Dialiteral = String

type DiaNumeral = Int

type MesNumeral = Int

type AñoNumeral = Int

type DiasDelMes = Int

type Mes = Int

queDiaEs :: DiaNumeral -> Dialiteral
queDiaEs x = case x of
  1 -> "Domingo"
  2 -> "Lunes"
  3 -> "Martes"
  4 -> "Miercoles"
  5 -> "Jueves"
  6 -> "Viernes"
  7 -> "Sabado"
  _ -> "No se quedia es"

diasDelMes :: MesNumeral -> AñoNumeral -> DiasDelMes
diasDelMes m a =
  let esBisiesto = añoEsBisiesto a
   in case m of
        1 -> 31
        2 -> if esBisiesto then 29 else 28
        3 -> 31
        4 -> 30
        5 -> 31
        6 -> 30
        7 -> 31
        8 -> 31
        9 -> 30
        10 -> 31
        11 -> 30
        12 -> 31
        _ -> -1
  where
    añoEsBisiesto :: AñoNumeral -> Bool
    añoEsBisiesto a =
        let 
            p = mod a 400 == 0 
            q = mod a 100 == 0
            r = mod a 4 == 0 
        in p && ( not q || r)

mas :: Int -> Int -> Int
mas a b = a + b

--  acento invertido, cambia el modo de dar las constantes, osea a 'mas' b = a 'mod' b 
-- 
-- case of es parecido al switch, con flecha
-- let in sirve para funciones intermediarias o simular una variable interna, pero si o si tiene que tener un in