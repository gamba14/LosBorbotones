module HIT
(
    Archivo(..),
    RevisionArchivo(..),
    tamanioArchivo,
    esVacio,
    cantLineas,
    algunaLineaEnBlanco,
    esHs,
    renombrarArchivo, 
    agregarLinea,
    quitarLinea,
    reemplazarLinea,
    wrappearArchivo,
    esModificacionInutil,
    buscarYReemplazarPalabra,
    aplicarRevision,
    buscarRevision,
    aplicarRevisionDirectorio,
    cualEsMasGrande
)
where

import Data.List
import Data.Char
import Data.String
import Data.Ord

import StringsAux


data Archivo = Archivo {nombre :: String , contenido :: String} deriving (Show, Eq)

type Directorio = [Archivo]

type Modificacion = Archivo -> Archivo

type Revision = [Modificacion]

data RevisionArchivo = RevisionArchivo {nombreArchivo :: String, revision :: Revision}

type RevisionDirectorio = [RevisionArchivo]


unTpGrupal :: Archivo

unTpGrupal = Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola " 

lineasLargas = Archivo 
                "lineasLargas.txt" 
                "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\n12345678901234567890" 


-- Funciones del tp

tamanioArchivo :: Archivo -> Int

tamanioArchivo archivo = length (contenido archivo) * 8 


esVacio :: Archivo -> Bool

esVacio archivo = (tamanioArchivo archivo) == 0


cantLineas :: Archivo -> Int

cantLineas (Archivo _ contenido) = length (lines contenido)


esLineaEnBlanco :: String -> Bool

esLineaEnBlanco linea = all isSpace linea


algunaLineaEnBlanco :: Archivo -> Bool

algunaLineaEnBlanco (Archivo nombre contenido) = any esLineaEnBlanco (lines contenido)


esHs :: Archivo -> Bool

esHs (Archivo nombre _) = drop (length nombre - 3) nombre == ".hs"


renombrarArchivo ::  String -> Archivo -> Archivo

renombrarArchivo nuevoNombre (Archivo _ contenido) = Archivo nuevoNombre contenido 


-- Agregar linea (texto) desde de (n) en (archivo)
agregarLinea :: String -> Int -> Archivo -> Archivo

agregarLinea linea posicion (Archivo nombre contenido) =
    Archivo 
    nombre
    (
        let anterior  =  lineaAnterior   posicion      contenido
            posterior =  lineaPosterior (posicion - 1) contenido

        in case anterior of 
            []       ->                           (lineaSegura linea) ++ posterior
            anterior -> (lineaSegura anterior) ++ (lineaSegura linea) ++ posterior
    )

-- Quitar linea (n) en Archivo
quitarLinea :: Int -> Archivo -> Archivo

quitarLinea posicion (Archivo nombre contenido) = 
    Archivo 
    nombre
    (
        let anterior  =  lineaAnterior  posicion contenido 
            posterior =  lineaPosterior posicion contenido
        in anterior ++ posterior
    )

-- Reemplazar linea (n) por (texto) en (archivo)
reemplazarLinea :: Int -> String -> Archivo -> Archivo

reemplazarLinea posicion nuevalinea (Archivo nombre contenido) =  
    Archivo 
    nombre
    (
        let anterior  =  lineaAnterior  posicion contenido
            posterior =  lineaPosterior posicion contenido

        in case anterior of 
            []       ->                           (lineaSegura nuevalinea) ++ posterior
            anterior -> (lineaSegura anterior) ++ (lineaSegura nuevalinea) ++ posterior
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

wrappearArchivo (Archivo nombre contenido) = 
    Archivo
    nombre
    (deslinear (map wrappearLinea (lineas contenido)))
        

esModificacionInutil :: Modificacion -> Archivo -> Bool

esModificacionInutil modificacion archivo = 
    archivoModificado == archivo
    where archivoModificado = modificacion archivo


reemplazarSiCoincide :: String -> String -> String -> String

reemplazarSiCoincide buscada porReemplazar palabra 
    | buscada == palabra = porReemplazar
    | otherwise = palabra


buscarEnLineaYRemplazar :: String -> String -> String -> String

buscarEnLineaYRemplazar buscada porReemplazar linea = unirSimbolos(map (reemplazarSiCoincide buscada porReemplazar) (dividirSimbolos linea))


buscarYReemplazarPalabra :: String -> String -> Archivo -> Archivo

buscarYReemplazarPalabra buscada porReemplazar (Archivo nombre contenido) = 
    Archivo 
        nombre
        (deslinear (map (buscarEnLineaYRemplazar buscada porReemplazar) (lineas contenido)))


-- Busca las revisiones para un archivo en una revision de directorio
buscarRevision :: RevisionDirectorio -> Archivo -> Revision

buscarRevision revisionD (Archivo nombre contenido) = foldl (++) [] (map revision (filter (\x -> (nombreArchivo x) == nombre) revisionD))


aplicarRevision :: Revision -> Archivo -> Archivo

aplicarRevision revision archivo = foldl (\arch modificacion -> modificacion arch) archivo revision


aplicarRevisionD :: RevisionDirectorio -> Archivo -> Archivo

aplicarRevisionD revisionD archivo = aplicarRevision (buscarRevision revisionD archivo) archivo


aplicarRevisionDirectorio :: RevisionDirectorio -> Directorio -> Directorio

aplicarRevisionDirectorio revisionD directorio = map (\archivo -> aplicarRevisionD revisionD archivo) directorio


cualEsMasGrande :: Directorio -> RevisionDirectorio -> Maybe Archivo 

-- Si el directorio esta vacio, interpretar como ninguno
cualEsMasGrande [] _ = Nothing

cualEsMasGrande directorio revisionDirectorio = 
    Just (
        maximumBy 
            (\a b -> comparing tamanioArchivo a b) 
            (aplicarRevisionDirectorio revisionDirectorio directorio)
    )



