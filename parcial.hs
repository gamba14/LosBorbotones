import  Data.List 


type Contexto = [(String, Float)]

data Alumno = Alumno {nombre :: String, actitud:: String, indice :: Float, recuerdos::[String]} deriving (Show, Eq)

data Profesor = Profesor 
    {
        nombreP :: String, 
        apreciacionContexto :: (Contexto -> Bool), 
        reaccion :: (Alumno -> Alumno) 
    } 


actitudesFrecuentes :: [String]

actitudesFrecuentes = ["buena onda", "predipuesto", "indiferente"]


contextoHipotetico :: [(String, Float)]

contextoHipotetico = 
    [
        ("aumento salarial"               ,   30.0 ), 
        ("presupuesto"                    , 2000.0 ), 
        ("represion de la protesta social",    1.18), 
        ("Inflacion"                      ,   35.5 )
    ]

cachito = Alumno 
            "Estalisnao pereira" 
            "buena onda" 
            15 
            [
                "conoci la facultad", 
                "aprobe mi primer examen", 
                "hice un hola mundo que funcionaba", 
                "conoci a julia"
            ]




averiguarSiAprobo alumno = any ((==) "aprobe") (map (take 6) (recuerdos alumno))

aumentarMotivacion alumno = Alumno 
                                (nombre alumno) 
                                (actitud alumno)
                                ((indice alumno) * 1.1)
                                (recuerdos alumno)

seraBuenProfesional alumno = (averiguarSiAprobo alumno) && (indice alumno >= 15)

experiencia alumno 

    | (motivacion == 0) || (actitud alumno == "indiferente") = alumno


    | motivacion > 5 && aprobo = aumentarMotivacion alumno


    | not aprobo = Alumno 
                    (nombre alumno)
                    "deprimido"
                    0
                    (recuerdos alumno)

    | otherwise = Alumno 
                    (nombre alumno)
                    "D:"
                    10
                    ((recuerdos alumno) ++ ["aprobe algo :D"])
    
    where   motivacion  = indice            alumno
            aprobo      = averiguarSiAprobo alumno 

-- XXX
-- obtenerIndicador contexto situacion = snd (find (\x -> situacion == fst x) contexto)

unDiaEspecial :: Contexto -> [Alumno] -> Profesor -> [Alumno]

unDiaEspecial contexto alumnos profesor

    | (apreciacionContexto profesor) contexto  = map (reaccion profesor) alumnos
 
    | otherwise = alumnos
