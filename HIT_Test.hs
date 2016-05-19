import HIT
import Tester


unTpGrupal = Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola " 

apuntesDeLamateria = 
    Archivo 
        "Apuntes de la materia.txt" 
        "Declarar el tipo de una funcion\n\tf :: tipo_del_parametro_1 ->  tipo_del_parametro_2 ->  tipo_de_la_imagen_de_f\n..." 

archivoPolemico = 
    Archivo
        "Este archivo no contiene nada que pueda ofender al lector.txt"
        ""

-- Tabla de pruebas (test)
tablasDePrueba = 
    [ 
        (
            generarTest 
                "Prueba para tamanioArchivo" 
                [show unTpGrupal] 
                536
                (tamanioArchivo unTpGrupal)
        ),
        (
            generarTest
                "Prueba de esVacio (1 de 2)"
                [show archivoPolemico]
                True 
                (esVacio archivoPolemico)
        ),
        (
            generarTest
                "Prueba de esVacio (2 de 2)"
                [show unTpGrupal]
                False
                (esVacio unTpGrupal)
        ),
        (
            generarTest
                "Prueba para cantLineas (1 de 3)"
                [show unTpGrupal]
                4
                (cantLineas unTpGrupal)
        ),
        (
            generarTest
                "Prueba para cantLineas (2 de 3)"
                [show archivoPolemico]
                0 -- TODO esto esta bien??
                (cantLineas archivoPolemico)
        ),
        (
            generarTest
                "Prueba para cantLineas (3 de 3)"
                [show apuntesDeLamateria]
                3
                (cantLineas apuntesDeLamateria)
        ),
        (
            generarTest
                "Prueba para algunaLineaEnBlanco (1 de 3)"
                [show unTpGrupal]
                True
                (algunaLineaEnBlanco unTpGrupal)
        ),
        (
            generarTest
                "Prueba para algunaLineaEnBlanco (2 de 3)"
                [show apuntesDeLamateria]
                False
                (algunaLineaEnBlanco apuntesDeLamateria)
        ),
        (
            generarTest
                "Prueba para algunaLineaEnBlanco (3 de 3)"
                [show archivoPolemico]
                False -- TODO muy polemico el archivo. esta bien esto??
                (algunaLineaEnBlanco archivoPolemico) 
        ),
        (
            generarTest
                "Prueba para esHs (1 de 2)"
                [show apuntesDeLamateria]
                False
                (esHs apuntesDeLamateria) 
        ),
        (
            generarTest
                "Prueba para esHs (2 de 2)"
                [show unTpGrupal]
                True
                (esHs unTpGrupal) 
        ),
        (
            generarTest
                "Prueba para renombrarArchivo"
                ["tpGrupal - version 5 final final 2 (para entregar).hs", show unTpGrupal]
                (Archivo "tpGrupal - version 5 final final 2 (para entregar).hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola ")
                (renombrarArchivo "tpGrupal - version 5 final final 2 (para entregar).hs" unTpGrupal) 
        )

    ] -- TODO: Agregar Pruebas

ejecutarPruebas = correrLasSiguientesPruebas tablasDePrueba 
