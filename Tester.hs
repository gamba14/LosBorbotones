module Tester
(
    Test(..),
    generarTest,
    correrLasSiguientesPruebas
)
where


data Test = Test {
    nombre      :: String,
    parametros  :: [String],
    esperado    :: String,
    obtenido    :: String,
    resultado   :: Bool

} deriving (Show, Eq)

-- Genera una prueba (test)
generarTest :: (Show r, Eq r) => String -> [String] -> r -> r -> Test

generarTest nombre parametros esperado funcionAplicada = 
    Test
        nombre
        parametros
        (show esperado)
        (show funcionAplicada)
        (funcionAplicada == esperado)
    

-- Genera el mensaje a mostrar en consola
generarMensaje :: Test -> String

generarMensaje test =
    "\tPrueba para " ++ nombre test ++ ":\t\t\t" ++ 
        case resultado test of   
            True  ->"Paso\n"

            False ->"Fallo\n\n" ++
                "\t\tCon el/los parametros:" ++ (show (parametros test))  ++ "\n" ++
                "\t\tSe esperaba:\t"         ++ esperado test             ++ "\n" ++
                "\t\tSe obtubo:\t"           ++ obtenido  test            ++ "\n\n"


-- Al llamarlo evalua e imprime los resultados de las pruebas (test)
correrLasSiguientesPruebas :: [Test] -> IO()

correrLasSiguientesPruebas tablasDePrueba = mapM_ putStr (map generarMensaje tablasDePrueba)
