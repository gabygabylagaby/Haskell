{--module Main where




data Estudiante = Estudiante(
    nombre:: String,
    segundoNombre:: String,
    apellidoPaterno:: String,
    apellidoMaterno:: String

) deriving(Show)

estudiantes :: [Estudiante]
estudiantes = [
    Estudiante "Pedro" "" "Perez" "Velasquez",
    Estudiante "Pedro" "" "Perez" "Velasquez",

]

seleccionarEstudiante :: [Estudiante] -> Estudiante
seleccionarEstudiante estudiante = let 
                                        indice = randomRIO  (0, length estudiante - 1)
                                    in 
                                        estudiantes !! indice 

main :: IO ()
main = do 
        putStrLn seleccionarEstudiante 
        -}