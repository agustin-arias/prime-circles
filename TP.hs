type Circulo = [Integer]

-- Ej1

{-
rotar toma un circulo [a,b,c] y un entero positivo k y rota el circulo k veces a la derecha.
Ejemplo:
rotar [1,2,3] 1 = [2,3,1]
rotar [1,2,3,4,5,6] 3 = [4,5,6,1,2,3].
-}
rotar :: Circulo -> Integer -> Circulo
rotar (x:xs) 1 = xs ++ [x]
rotar (x:xs) k = rotar (xs ++ [x]) (k-1)


{-longitud toma un Circulo y le calcula la longitud-}
longitud :: Circulo -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


{-sonCirculosIgualesprevia toma dos circulos xs,ys y tambien un entero k y se fija si xs es igual a "rotar ys j". Con j desde k hasta la longitud de ys -}
sonCirculosIgualesprevia :: Circulo -> Circulo -> Integer -> Bool
sonCirculosIgualesprevia xs ys k 
 | k == longitud ys = (xs == ys) -- rotar ys (longitud ys) = ys
 | xs == rotar ys k = True
 | otherwise = sonCirculosIgualesprevia xs ys (k+1)


{-sonCirculosIguales toma dos circulos xs ys y se fija si son iguales-}
sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales xs ys = sonCirculosIgualesprevia xs ys 1




-- Ej3
{--}
menorDivisorDeNmayorqueK :: Integer -> Integer -> Integer
menorDivisorDeNmayorqueK n k
 | (mod n k == 0) = k
 | otherwise = menorDivisorDeNmayorqueK n (k+1)


{--}
menorDivisor n = menorDivisorDeNmayorqueK n 2


{--}
esPrimo :: Integer -> Bool
esPrimo p = (menorDivisor p == p)


{-ultimoElemento calcula el ultimo elemento de un Circulo, con respecto a donde se decidio comenzar a contar. Ya que los circulos no tienen ultimo elemento, se los representa por medio de una lista eligiendo arbitrariamente algun numero y agregando los que esten en sentido horario hasta completar el circulo, ultimoElemento devuelve el numero que esta mas proximo en sentido antihorario al numero elegido como primer elemento de la lista que representa al Circulo-}
ultimoElemento :: Circulo -> Integer
ultimoElemento [x] = x
ultimoElemento xs = ultimoElemento (tail xs)

{-esCirculoPrimoprevia toma un circulo y determina si dado la representacion [a_1,..,a_n], la suma a_i+a_{i+1} es primo para i=1,..,n-1 -}
esCirculoPrimoprevia :: Circulo -> Bool
esCirculoPrimoprevia (x:y:xs)
 | xs == [] = esPrimo (x+y)
 | otherwise = esPrimo(x+y) && (esCirculoPrimoprevia (y:xs))


{-esCirculoPrimo toma un circulo de la forma [a_1,a_2,...a_n] y se fija si a_i+a_{i+1} es un primo para i = 1,..,n-1 y si a_n + a_1 es tambien primo-}
esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo (x:xs)
 | not(esPrimo (x + ultimoElemento (x:xs))) = False -- Nos fijamos si a_1+a_n es primo, si no lo es el circulo no es Primo 
 | otherwise = esCirculoPrimoprevia (x:xs)


