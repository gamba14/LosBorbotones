import Data.List
import Data.Char

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

