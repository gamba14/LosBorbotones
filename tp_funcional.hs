import Data.List
import Data.Char
data Archivo = Archivo {nombre :: String , contenido :: String} deriving (Show, Eq)

unTpGrupal :: Archivo

unTpGrupal = Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola " 

tamanioArchivo archivo = length(contenido archivo)*8 

esVacio archivo = length(contenido archivo) == 0

cantLineas archivo = length(lines(contenido archivo))

--esLineaEnBlanco archivo =  

esHs archivo = drop (length(nombre archivo)-3) (nombre archivo) == ".hs"

renombrarArchivo archivo nuevo = Archivo nuevo (contenido archivo) 

