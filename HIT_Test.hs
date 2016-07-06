import HIT
import Tester


-- Archivos

unTpGrupal = Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n hola " 

apuntesDeLamateria = 
    Archivo 
        "Apuntes de la materia.txt" 
        "Declarar el tipo de una funcion\n\tf :: tipo_del_parametro_1 ->  tipo_del_parametro_2 ->  tipo_de_la_imagen_de_f\n..." 

archivoPolemico = 
    Archivo
        "Este archivo no contiene nada que pueda ofender al lector.txt"
        ""

lineasLargas = 
    Archivo 
        "lineasLargas.txt" 
        "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\n12345678901234567890" 

-- Revision para tp grupal A
revisionTpGrupalA =  
    [
        (agregarLinea " listaLarga = (>999) . length " 2), 
        (quitarLinea 3), 
        (quitarLinea 3), 
        (quitarLinea 3)
    ] 

-- Como no se pueden mostar las funciones uso un String como workarrond
showRevisionTpGrupalA = 
    "["                                                         ++
        "(agregarLinea \" listaLarga = (>999) . length \" 2),"  ++
        "(quitarLinea 3), "                                     ++
        "(quitarLinea 3), "                                     ++
        "(quitarLinea 3) "                                      ++
    "]"

-- unTpGrupal depues de aplicar la revicion A
unTpGrupalrA = Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>999) . length \n"


-- Revision para tp grupal B
revisionTpGrupalB = 
    [
        (agregarLinea " listaLarga = (>999) . length " 2), 
        wrappearArchivo, 
        (reemplazarLinea 3 "\n"), 
        (quitarLinea 4),
        (agregarLinea " print \"hola mundo\"" 3),
        (quitarLinea 5)
    ]

-- Como no se pueden mostar las funciones uso un String como workarrond
showRevisionTpGrupalB = 
    "["                                                         ++ 
        "(agregarLinea \" listaLarga = (>999) . length \" 2), " ++
        " wrappearArchivo, "                                    ++ 
        "(reemplazarLinea 3 \"\n\"), "                          ++
        "(quitarLinea 4), "                                     ++
        "(agregarLinea 3 \" print \"hola mundo\"\"), "          ++ 
        "(quitarLinea 5)"                                       ++ 
    "]" 
                  
-- unTpGrupal depues de aplicar la revicion B
unTpGrupalrB = Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>999) . length \n print \"hola mundo\"\n\n"


-- Revision para tp lineas Largas
revicionLineasLargas = 
    [
        (
            agregarLinea 
                "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
                5 
        )
    ]

-- Como no se pueden mostar las funciones uso un String como workarrond
showRevicionLineasLargas =
    "[" ++
        "(" ++
            "agregarLinea" ++
                "\"123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\"" ++
                "5" ++
        ")" ++
    "]"

lineasLargasrC = 
    Archivo 
        "lineasLargas.txt" 
        ( 
          "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\n" ++ 
          "12345678901234567890\n" ++
          "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\n" 
        )


-- Directorio
directorioDePruebas = [ unTpGrupal, apuntesDeLamateria, archivoPolemico, lineasLargas]


revisionDirectorioA = [ RevisionArchivo "tpGrupal.hs" revisionTpGrupalA]

revisionDirectorioB = [ RevisionArchivo "tpGrupal.hs" revisionTpGrupalB]

revisionDirectorioC = [ RevisionArchivo "lineasLargas.txt" revicionLineasLargas]


directorioDePruebasRA = [ unTpGrupalrA, apuntesDeLamateria, archivoPolemico, lineasLargas ]

directorioDePruebasRB = [ unTpGrupalrB, apuntesDeLamateria, archivoPolemico, lineasLargas ]

directorioDePruebasRC = [ unTpGrupal, apuntesDeLamateria, archivoPolemico, lineasLargasrC]


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
        ),
        (
            generarTest
                "Prueba para esModificacionInutil (1 de 3)"
                ["wrappearArchivo", show unTpGrupal]
                True
                (esModificacionInutil wrappearArchivo unTpGrupal)
        ),
        (
            generarTest
                "Prueba para esModificacionInutil (2 de 3)"
                ["wrappearArchivo", show lineasLargas]
                False
                (esModificacionInutil wrappearArchivo lineasLargas)
        ),
        (
            generarTest
                "Prueba para esModificacionInutil (3 de 3)"
                ["quitarLinea (-1) ", show unTpGrupal]
                True
                (esModificacionInutil (quitarLinea (-1)) unTpGrupal)
        ),
        (
            generarTest
                "Prueba para buscarYReemplazarPalabra (1 de 2)"
                ["hola", "mundo", show unTpGrupal]
                (Archivo "tpGrupal.hs" "listaLarga :: [a] -> Bool \n listaLarga = (>9) . length \n    \n mundo ")
                (buscarYReemplazarPalabra "hola" "mundo" unTpGrupal)
        ),
        (
            generarTest
                "Prueba para buscarYReemplazarPalabra (2 de 2)"
                ["nada", "todo", show unTpGrupal]
                unTpGrupal
                (buscarYReemplazarPalabra "nada" "todo" unTpGrupal)
        ),
        (
            generarTest
                "Prueba para aplicarRevision (1 de 2)"
                [
                    showRevisionTpGrupalA, 
                    show unTpGrupal
                ]
                unTpGrupalrA
                (aplicarRevision revisionTpGrupalA unTpGrupal)
        ),
        (
            generarTest
                "Prueba para aplicarRevision (2 de 2)"
                [
                    showRevisionTpGrupalB,
                    show unTpGrupal
                ]
                unTpGrupalrB
                ( aplicarRevision revisionTpGrupalB unTpGrupal )
        ),
        (
            generarTest
                "Prueba para aplicarRevisionDirectorio (1 de 4)"
                [
                    "[]",
                    "[]"
                ]
                []
                ( aplicarRevisionDirectorio [] [] )
        ),
        (
            generarTest
                "Prueba para aplicarRevisionDirectorio (2 de 4)"
                [
                    "[]",
                    show directorioDePruebas
                ]
                directorioDePruebas
                ( aplicarRevisionDirectorio [] directorioDePruebas )
        ),
        (
            generarTest
                "Prueba para aplicarRevisionDirectorio (3 de 4)"
                [
                    "revisionDirectorioA", -- fiaca para poner lo que va 
                    show directorioDePruebas
                ]
                directorioDePruebasRA
                ( aplicarRevisionDirectorio revisionDirectorioA directorioDePruebas )
        ),
        (
            generarTest
                "Prueba para aplicarRevisionDirectorio (4 de 4)"
                [
                    "revisionDirectorioC", -- fiaca para poner lo que va 
                    show directorioDePruebas
                ]
                directorioDePruebasRC
                ( aplicarRevisionDirectorio revisionDirectorioC directorioDePruebas )
        ),
        (
            generarTest
                "Prueba para cualEsMasGrande (1 de 1)"
                [
                    "[]",
                    "revisionDirectorioA" -- fiaca para poner lo que va 
                ]
                Nothing
                (cualEsMasGrande [] revisionDirectorioA)
        ),
        (
            generarTest
                "Prueba para cualEsMasGrande (2 de 2)"
                [
                    show directorioDePruebas,
                    "[]"
                ]
                (Just apuntesDeLamateria)
                (cualEsMasGrande directorioDePruebas [])
        ),
        (
            generarTest
                "Prueba para cualEsMasGrande (3 de 3)"
                [
                    show directorioDePruebas,
                    "revisionDirectorioA" -- fiaca para poner lo que va 
                ]
                (Just apuntesDeLamateria)
                (cualEsMasGrande directorioDePruebas revisionDirectorioA)
        ),
        (
            generarTest
                "Prueba para cualEsMasGrande (4 de 4)"
                [
                    show directorioDePruebas,
                    "revisionDirectorioC" -- fiaca para poner lo que va 
                ]
                (Just lineasLargasrC)
                (cualEsMasGrande directorioDePruebas revisionDirectorioC)
        ),
        (
            generarTest
                "Prueba para cualDiffiereMasEnTamanio (1 de ?)"
                [
                    "[]",
                    "revisionDirectorioA" -- fiaca para poner lo que va 
                ]
                Nothing
                (cualDiffiereMasEnTamanio [] revisionDirectorioA)
        ),
        (
            generarTest
                "Prueba para cualDiffiereMasEnTamanio (2 de ?)"
                [
                    show directorioDePruebas,
                    "revisionDirectorioC" -- fiaca para poner lo que va 
                ]
                (Just lineasLargasrC)
                (cualDiffiereMasEnTamanio directorioDePruebas revisionDirectorioC)
        )
    ] -- TODO: Agregar Pruebas

ejecutarPruebas = correrLasSiguientesPruebas tablasDePrueba 
