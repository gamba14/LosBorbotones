import HIT
import Tester


unTpGrupal = Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola " 

-- Tabla de pruebas (test)
tablasDePrueba = 
    [ 
        (
            generarTest 
                "tamanioArchivo" 
                [show unTpGrupal] 
                536
                (tamanioArchivo unTpGrupal)
        )
    ] -- TODO: Agregar Pruebas

ejecutarPruebas = correrLasSiguientesPruebas tablasDePrueba 