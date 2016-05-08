import Data.List
import Data.Char
import Data.String

data Archivo = Archivo {nombre :: String , contenido :: String} deriving (Show, Eq)

unTpGrupal :: Archivo

unTpGrupal = Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola " 


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


renombrarArchivo :: Archivo -> String -> Archivo

renombrarArchivo archivo nuevo = Archivo nuevo (contenido archivo) 


agregarLinea :: Archivo -> Int -> String -> Archivo

agregarLinea archivo posicion linea =
    Archivo 
    (nombre archivo) 
    (
        let anterior  =  unlines (take (posicion - 1) (lines (contenido archivo))) 
            posterior =  unlines (drop (posicion - 1) (lines (contenido archivo))) 

        in anterior ++ linea ++ "\n" ++ posterior
    )


quitarLinea :: Archivo -> Int -> Archivo

quitarLinea archivo posicion = 
    Archivo 
    (nombre archivo) 
    (
        let anterior  =  unlines (take (posicion - 1) (lines (contenido archivo))) 
            posterior =  unlines (drop (posicion    ) (lines (contenido archivo))) 
        in anterior ++ posterior
    )

reemplazarLinea :: Archivo -> Int -> String -> Archivo

reemplazarLinea archivo posicion nuevalinea = 
    Archivo 
    (nombre archivo) 
    (
        let anterior  =  unlines (take (posicion - 1) (lines (contenido archivo))) 
            posterior =  unlines (drop (posicion    ) (lines (contenido archivo))) 

        in anterior ++ nuevalinea ++ "\n" ++ posterior
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
-- FIXME: Agrega un salto de linea aunque no cambie nada (bug?)

wrappearLinea :: String -> String

wrappearLinea linea = wrappearLineaAux "" linea

    
wrappearArchivo :: Archivo -> Archivo

wrappearArchivo archivo = 
    Archivo
    (nombre archivo)
    (unlines (map wrappearLinea (lines (contenido archivo))))
        

type Modificacion = Archivo -> Archivo

esModificacionInutil :: Archivo -> Modificacion -> Bool

esModificacionInutil archivo modificacion = archivoModificado == archivo
    where archivoModificado = modificacion archivo


reemplazarSiCoincide buscada porReemplazar palabra | buscada == palabra = porReemplazar
                                                   | otherwise = palabra

buscarYReemplazar archivo buscada porReemplazar = 
    Archivo (nombre archivo )
    (unwords(map (reemplazarSiCoincide buscada porReemplazar) (words(contenido archivo)))) 