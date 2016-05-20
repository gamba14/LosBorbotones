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

lineasLargas = Archivo 
                "lineasLargas.txt" 
                "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\n12345678901234567890" 

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
        ),
        (
            generarTest
                "Prueba para agregarLinea (1 de 4)"
                ["Whose Line Is It Anyway?", show 2, show unTpGrupal]
                (Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \nWhose Line Is It Anyway?\n listaLarga = (>9) . length \n    \n hola ")
                (agregarLinea "Whose Line Is It Anyway?" 2 unTpGrupal)
        ),
        (
            generarTest
                "Prueba para agregarLinea (2 de 4)"
                ["Whose Line Is It Anyway?", show 5, show unTpGrupal]
                (Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola \nWhose Line Is It Anyway?\n")
                (agregarLinea "Whose Line Is It Anyway?" 5 unTpGrupal)
        ),
        (
            generarTest
                "Prueba para agregarLinea (3 de 4)"
                ["Whose Line Is It Anyway?", show 1, show unTpGrupal]
                (Archivo "tpGrupal.hs" "Whose Line Is It Anyway?\nlistaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola ")
                (agregarLinea "Whose Line Is It Anyway?" 1 unTpGrupal)
        ),
        (
            generarTest
                "Prueba para agregarLinea (4 de 4)"
                ["Whose Line Is It Anyway?", show (-1), show unTpGrupal]
                (Archivo "tpGrupal.hs" "Whose Line Is It Anyway?\nlistaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola ")
                (agregarLinea "Whose Line Is It Anyway?" (-1) unTpGrupal)
        ),
        (
            generarTest
                "Prueba para quitarLinea (1 de 4)"
                [show 3, show unTpGrupal]
                (Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n hola " )
                (quitarLinea 3 unTpGrupal)
        ),
        (
            generarTest
                "Prueba para quitarLinea (2 de 4)"
                [show 4, show unTpGrupal]
                (Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n" )
                (quitarLinea 4 unTpGrupal)
        ),
        (
            generarTest
                "Prueba para quitarLinea (3 de 4)"
                [show 5, show unTpGrupal]
                (Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola " )
                (quitarLinea 5 unTpGrupal)
        ),
        (
            generarTest
                "Prueba para quitarLinea (4 de 4)"
                [show (-1), show unTpGrupal]
                (Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola " )
                (quitarLinea (-1) unTpGrupal)
        ),
        (
            generarTest
                "Prueba para reemplazarLinea (1 de 4)"
                [show 3, "Keep yourself alive", show unTpGrupal]
                (Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \nKeep yourself alive\n hola " )
                (reemplazarLinea 3 "Keep yourself alive" unTpGrupal)
        ),
        (
            generarTest
                "Prueba para reemplazarLinea (2 de 4)"
                [show 3, "Keep yourself alive \n", show unTpGrupal]
                (Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \nKeep yourself alive \n hola " )
                (reemplazarLinea 3 "Keep yourself alive \n" unTpGrupal)
        ),
        (
            generarTest
                "Prueba para reemplazarLinea (3 de 4)"
                [show 7, "Keep yourself alive", show unTpGrupal]
                (Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola \nKeep yourself alive\n" )
                (reemplazarLinea 7 "Keep yourself alive" unTpGrupal)
        ),
        (
            generarTest
                "Prueba para reemplazarLinea (4 de 4)"
                [show (-1), "Keep yourself alive", show unTpGrupal]
                (Archivo "tpGrupal.hs" "Keep yourself alive\nlistaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola " )
                (reemplazarLinea (-1) "Keep yourself alive" unTpGrupal)
        ),
        (
            generarTest
                "Prueba para wrappearArchivo (1 de 4)"
                [show lineasLargas]
                (Archivo 
                    "lineasLargas.txt" 
                    "12345678901234567890123456789012345678901234567890123456789012345678901234567890\n1234567890\n12345678901234567890"
                )
                (wrappearArchivo lineasLargas)
        ),
        (
            generarTest
                "Prueba para wrappearArchivo (2 de 4)"
                [show archivoPolemico]
                archivoPolemico
                (wrappearArchivo archivoPolemico)
        ),
        (
            generarTest
                "Prueba para wrappearArchivo (3 de 4)"
                [show apuntesDeLamateria]
                apuntesDeLamateria
                (wrappearArchivo apuntesDeLamateria)
        ),
        (
            generarTest
                "Prueba para wrappearArchivo (4 de 4)"
                [show unTpGrupal]
                unTpGrupal
                (wrappearArchivo unTpGrupal)
        )



    ] -- TODO: Agregar Pruebas

ejecutarPruebas = correrLasSiguientesPruebas tablasDePrueba 
