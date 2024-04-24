-- comentario de una linea
{--

varias lineas de comentario

--}

miNumeroFavorito :: Integer
miNumeroFavorito = 2

miNumeroDecimalFvorito :: Double
miNumeroDecimalFvorito = 2.57

miInicial :: Char
miInicial = 'A'

tuNombre :: [Char]
tuNombre = "Gervasio"

tuOtroNombre :: [Char]
tuOtroNombre = ['E','m','i','l','a']

listaDeNumeros :: [Integer]
listaDeNumeros = [1,2,3]

tuplaDeCoordenadas :: (Integer, Integer)
tuplaDeCoordenadas = (3, 2)

anadirElementoALista :: [a] -> a -> [a]
anadirElementoALista l e = e : l

sacarElementoDeUnaLista :: [a] -> a
sacarElementoDeUnaLista (e:l) = e

listaDeUnoAlCien = [1 .. 100]
listaDeNumerosPAresDeUnaAlCien = [0, 2 .. 100]

listaDeNumerosImpares = [1, 3 .. ]

multiplesDeTres = [n | n <- [1..], mod n 3 == 0]
