import Data.Char (ord)
import Data.List (splitOn)

esVocal letra | letra == 'a' = True
              | letra == 'e' = True
              | letra == 'i' = True
              | letra == 'o' = True
              | letra == 'u' = True
              | otherwise = False

esConsonante letra =  esLetra letra && not(esVocal letra)

esLetra letra   | ord(letra) >=  65 && ord(letra) <= 90 = True
                | ord(letra) >= 97 && ord(letra) <= 122 = True
                | otherwise = False

cantVocales palabra = length(filter esVocal palabra)

cantConsonantes palabra = length(filter esConsonante palabra)

masVocqueCons frase = cantConsonantes frase > cantVocales frase

extraerSigla frase = map head frase 

