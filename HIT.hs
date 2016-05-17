module HIT
(
    tamanioArchivo,
    esVacio,
    cantLineas,
    esLineaEnBlanco,
    algunaLineaEnBlanco,
    esHs,
    renombrarArchivo, 
    agregarLinea,
    quitarLinea,
    reemplazarLinea,
    wrappearArchivo,
    esModificacionInutil,
    buscarYReemplazar
)
where

import Data.List
import Data.Char
import Data.String

import StringsAux


data Archivo = Archivo {nombre :: String , contenido :: String} deriving (Show, Eq)

type Modificacion = Archivo -> Archivo

unTpGrupal :: Archivo

unTpGrupal = Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola " 

lineasLargas = Archivo 
                "lineasLargas.txt" 
                "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\n12345678901234567890" 


-- Funciones del tp

tamanioArchivo :: Archivo -> Int

tamanioArchivo archivo = length (contenido archivo) * 8 


esVacio :: Archivo -> Bool

esVacio archivo = length (contenido archivo) == 0


cantLineas :: Archivo -> Int

cantLineas archivo = length (lines (contenido archivo))


esLineaEnBlanco :: String -> Bool

esLineaEnBlanco linea = all isSpace linea


algunaLineaEnBlanco :: Archivo -> Bool

algunaLineaEnBlanco archivo = any esLineaEnBlanco (lines (contenido archivo))


esHs :: Archivo -> Bool

esHs archivo = drop (length (nombre archivo) - 3) (nombre archivo) == ".hs"


renombrarArchivo ::  String -> Archivo -> Archivo

renombrarArchivo nuevoNombre archivo = Archivo nuevoNombre (contenido archivo) 

-- Agregar linea (texto) desde de (n) en (archivo)
agregarLinea :: String -> Int -> Archivo -> Archivo

agregarLinea linea posicion archivo=
    Archivo 
    (nombre archivo) 
    (
        let anterior  =  deslinear (take (posicion - 1) (lineas (contenido archivo))) 
            posterior =  deslinear (drop (posicion - 1) (lineas (contenido archivo)))  

        in anterior ++ (lineaSegura linea) ++ posterior
    )

-- Quitar linea (n) en Archivo
quitarLinea :: Int -> Archivo -> Archivo

quitarLinea posicion archivo = 
    Archivo 
    (nombre archivo) 
    (
        let anterior  =  deslinear (take (posicion - 1) (lineas (contenido archivo))) 
            posterior =  deslinear (drop (posicion    ) (lineas (contenido archivo))) 
        in anterior ++ posterior
    )

-- Reemplazar linea (n) por (texto) en (archivo)
reemplazarLinea :: Int -> String -> Archivo -> Archivo

reemplazarLinea posicion nuevalinea archivo =  
    Archivo 
    (nombre archivo) 
    (
        let anterior  =  deslinear (take (posicion - 1) (lineas (contenido archivo))) 
            posterior =  deslinear (drop (posicion    ) (lineas (contenido archivo))) 

        in anterior ++ (lineaSegura nuevalinea) ++ posterior
    )



wrappearLineaAux :: String -> String -> String

wrappearLineaAux wrappeadas porWrappear 
    | (length porWrappear) >= 80 = 
        (
            let anterior  = take 80 porWrappear
                posterior = drop 80 porWrappear
            in  wrappearLineaAux (wrappeadas ++ anterior ++ "\n") posterior
        )
    | otherwise = wrappeadas ++ porWrappear


wrappearLinea :: String -> String

wrappearLinea linea = wrappearLineaAux "" linea

    
wrappearArchivo :: Archivo -> Archivo

wrappearArchivo archivo = 
    Archivo
    (nombre archivo)
    (deslinear (map wrappearLinea (lineas (contenido archivo))))
        

esModificacionInutil :: Modificacion -> Archivo -> Bool

esModificacionInutil modificacion archivo = archivoModificado == archivo
    where archivoModificado = modificacion archivo


reemplazarSiCoincide :: String -> String -> String -> String

reemplazarSiCoincide buscada porReemplazar palabra | buscada == palabra = porReemplazar
                                                   | otherwise = palabra


buscarEnLineaYRemplazar :: String -> String -> String -> String

buscarEnLineaYRemplazar buscada porReemplazar linea = unirSimbolos(map (reemplazarSiCoincide buscada porReemplazar) (dividirSimbolos linea))


buscarYReemplazar :: String -> String -> Archivo -> Archivo

buscarYReemplazar buscada porReemplazar archivo = 
    Archivo (nombre archivo)
    (unlines (map (buscarEnLineaYRemplazar buscada porReemplazar) (lines (contenido archivo)) ))

