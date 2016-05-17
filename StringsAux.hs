module StringsAux
(
    dividirSimbolos,
    unirSimbolos,
    lineas, 
    deslinear,
    lineaSegura
)
where

import Data.Char

-- Auxiliar de dividir Simbolos
dividirSimbolosAux :: String -> String -> [String] -> [String]

dividirSimbolosAux [] "" simbolos = simbolos

dividirSimbolosAux [] leido simbolos = simbolos ++ [leido]

dividirSimbolosAux (letra:resto) leido simbolos

        | isSpace letra && not (null leido) = 
            dividirSimbolosAux 
                resto
                "" 
                (simbolos ++ [leido] ++ [[letra]]) 

        | isSpace letra && null leido = 
            dividirSimbolosAux 
                resto
                "" 
                (simbolos ++ [[letra]])            

        | otherwise = 
            dividirSimbolosAux 
                resto
                (leido ++ [letra]) 
                simbolos 


-- Divide un texto en una lista, usando como referencia los caracteres
-- en del tipo espacio e incluyendolos en la lista.
-- Fu## you 'words' function
dividirSimbolos :: String -> [String]

dividirSimbolos []    = []
dividirSimbolos texto = dividirSimbolosAux texto "" []


-- Funcion inverza de dividirSimbolos
-- Fu## you 'unwords' function
unirSimbolos :: [String] -> String

unirSimbolos simbolos = foldr (++) "" simbolos


-- Auxiliar para lineas
lineasAux :: String -> String -> [String] -> [String]

lineasAux [] "" resultado = resultado

lineasAux [] leido resultado = resultado ++ [leido]

lineasAux (letra:resto) leido resultado
    | letra == '\n' = lineasAux resto "" (resultado ++ [(leido ++ "\n")])
    | otherwise     = lineasAux resto (leido ++ [letra])  resultado


-- Como lines pero mantiene el simbolo de salto de linea '\n'
lineas :: String -> [String]

lineas [] = []

lineas texto = lineasAux texto "" []

-- Funcion inverza de lineas y estupido juego de palabras
deslinear :: [String] -> String

-- Misma implementacion que unirSimbolos pero lo mantengo separado por si me da
-- ganas de cambiar la definicion de lineas (?)
deslinear lineas = foldr (++) "" lineas


-- Dado un texto verifica si termina con el caracter de nueva linea "\n" 
-- sino es el caso agrega un "\n" al final
-- "Linea Segura" o "Asegurame que sea una linea", chistonto ;D
lineaSegura :: String -> String

lineaSegura texto 
    | (last texto) == '\n' = texto
    | otherwise = texto ++ "\n"
