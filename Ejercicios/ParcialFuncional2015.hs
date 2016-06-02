

type Caracteristica = (String, Float)

data TerritorioGobernable = TerritorioGobernable 
    {
        nombre :: String, 
        poblacion :: Int, 
        caracteristicas :: [Caracteristica]
    } 
    deriving (Show, Eq)

type FuerzaPolitica = TerritorioGobernable -> TerritorioGobernable

-- Paises para el testing (manual)

miPais = TerritorioGobernable
    "argentina"
    40000000
    [ 
        ("desocupacion",        12.4), 
        ("educacion",         1000.0), 
        ("deuda externa", 80000000.0), 
        ("iva",                 21.0), 
        ("reservas",      30000000.0)
    ]

microPais = TerritorioGobernable
    "Principado de SeaLand"
    22
    [ 
        ("desocupacion",             0.0), 
        ("educacion",               22.0), 
        ("deuda externa",            0.0), 
        ("iva",                      0.0), 
        ("reservas",            600000.0),
        ("estamos apretados",       22.5)
    ]

superPais = TerritorioGobernable
    "SuperEstan"
    40000000
    [ 
        ("desocupacion",               0.4), 
        ("educacion",               9999.0), 
        ("deuda externa",              0.0), 
        ("iva",                       10.0), 
        ("reservas",        700000000000.0),
        ("investigaciones",      7000000.0),
        ("joputes",                    0.0),
        ("hipotetisidad",            100.0),
        ("ambision per capital",       5.0),
        ("podriamos estar mejor",    500.0)
    ]

estanMalPais = TerritorioGobernable
    "JodidosEstan"
    40000000
    [ 
        ("desocupacion",            100.0), 
        ("educacion",                 0.1), 
        ("deuda externa",     400000000.0), 
        ("iva",                       5.0), 
        ("reservas",                  0.0), 
        ("negacion de la realidad", 500.0)
    ]

-- Funciones de ayuda NO asociadas a una funcion en espefico (?)

obtenerIndice :: String -> TerritorioGobernable -> Float

obtenerIndice indice territorio
        | caracts == [] = -9e99  -- absurdo
        | otherwise     =  head $ map snd $ caracts 

    where caracts = filter (((==) indice) . fst) (caracteristicas territorio)

-- Puntos del parcial

-- Tecnicamente una FuerzaPolitica (Naturalismo?)
crecimientoVegetativo :: TerritorioGobernable -> TerritorioGobernable

crecimientoVegetativo territorio =  
    TerritorioGobernable
        (nombre territorio)
        (((poblacion territorio) * 105) `div` 100)
        (caracteristicas territorio)


-- Hacer una función para analizar si el país está bien en un determinado momento, 
-- que consiste en ver si la deuda externa por habitante es mayor al iva vigente.
-- Definido por un habitante de JodidosEstan
estanBien :: TerritorioGobernable -> Bool

estanBien territorio = (deudaExterna / habitantes) > iva
    where   deudaExterna = obtenerIndice "deuda externa" territorio
            iva          = obtenerIndice "iva"           territorio
            habitantes   = fromIntegral $ poblacion $ territorio


-- Definido por un "economista" mediatico
futuroPrometedor :: TerritorioGobernable -> Bool

futuroPrometedor = estanBien . crecimientoVegetativo . crecimientoVegetativo 


-- Las cosas cambian para bien, mal o pior
cambiarCaracteristica :: String -> (Float -> Float) -> TerritorioGobernable -> TerritorioGobernable

cambiarCaracteristica indice transformacion territorio =
    TerritorioGobernable
        (nombre    territorio)
        (poblacion territorio)
        (sinCambiar ++ cambiadas)
    
    where sinCambiar = filter (((/=) indice) . fst) (caracteristicas territorio)
          cambiadas  = [(indice, transformacion $ obtenerIndice indice territorio)]

-- Fuerzas Politicas




-- Danielismo: Baja un punto la desocupacion y mejora la educación en un 10%.
danielismo :: FuerzaPolitica

danielismo = (cambiarCaracteristica "desocupacion" (\x -> x - 1.0)) . (cambiarCaracteristica "educacion" (\x -> x * 1.1))


-- Margaritismo: Mejora la educación en un 40% y vuelve el iva al 18.
margaritismo :: FuerzaPolitica

margaritismo = (cambiarCaracteristica "educacion" (\x -> 1.4)) . (cambiarCaracteristica "iva" (\x -> 18))


-- Auxiliar para mauricismo
caracteristicasConD :: TerritorioGobernable -> [String]

caracteristicasConD = (filter $ (\ c -> c == 'd' || c == 'D') . head) . (map fst) . caracteristicas 

-- Mauricismo: Todo lo que empieza con 'd' lo duplica.
mauricismo :: FuerzaPolitica

mauricismo territorio = foldl cambiar territorio (caracteristicasConD territorio)

    where cambiar tg i = cambiarCaracteristica i (\x -> 2.0 * x) tg


-- Sergismo: Aumenta todo en un 10%
sergismo :: FuerzaPolitica

-- NO puedo hacer DRY a esta hora
sergismo territorio = foldl aumentar territorio ((map fst) $ caracteristicas $ territorio)

    where aumentar tg i = cambiarCaracteristica i (\x -> 1.1 * x) tg


-- Nicolismo: Hace que la deuda externa se anule (que sea 0).  
nicolismo :: FuerzaPolitica

nicolismo = cambiarCaracteristica "deuda externa" (\x -> 0.0)

-- Adolfismo: Deja todo igual
adolfismo :: FuerzaPolitica

adolfismo = id

-- Parte 3

periodoDeGobierno :: FuerzaPolitica -> TerritorioGobernable -> TerritorioGobernable

periodoDeGobierno f = crecimientoVegetativo . f


-- TODO:faltan 2 puntos triviales, uno es necesario para testear el ultimo

comoVaAQuedarElPais :: FuerzaPolitica -> TerritorioGobernable -> TerritorioGobernable

-- TODO: Test me
comoVaAQuedarElPais f tg
    | estanBien primerPeriodo = periodoDeGobierno f tg
    | otherwise               = primerPeriodo
    where primerPeriodo = periodoDeGobierno f tg

