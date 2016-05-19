import StringsAux
import Tester


-- Tabla de pruebas (test)
tablasDePrueba = 
    [ 
        (
            generarTest 
                "Prueba para dividirSimbolos" 
                ["asdf\r\n\thola "] 
                ["asdf","\r","\n","\t","hola"," "] 
                (dividirSimbolos "asdf\r\n\thola ")
        ),
        (
            generarTest
                "Prueba para unirSimbolos" 
                [show ["asdf","\r","\n","\t","hola"," "]]
                "asdf\r\n\thola " 
                (unirSimbolos ["asdf","\r","\n","\t","hola"," "])
        ),
        (
            generarTest
                "Prueba para lineas (1 de 3)" 
                ["line 1 \n linea 2\nlinea 3\t\n ultima linea sin line feed"]
                ["line 1 \n", " linea 2\n", "linea 3\t\n"," ultima linea sin line feed"]
                (lineas "line 1 \n linea 2\nlinea 3\t\n ultima linea sin line feed")
        ),
        (
            generarTest
                "Prueba para lineas (2 de 3)" 
                ["line 1 \n"]
                ["line 1 \n"]
                (lineas "line 1 \n")
        ),
        (
            generarTest
                "Prueba para lineas (3 de 3)" 
                ["line 1"]
                ["line 1"]
                (lineas "line 1")
        ),
        (
            generarTest
                "Prueba para deslinear (1 de 3)" 
                [show ["line 1"]]
                "line 1"
                (deslinear ["line 1"])
        ),(
            generarTest
                "Prueba para deslinear (2 de 3)" 
                [ show ["line 1 \n"]]
                "line 1 \n"
                (deslinear ["line 1 \n"])
        ),
        (
            generarTest
                "Prueba para deslinear (3 de 3)" 
                [show ["line 1 \n", " linea 2\n", "linea 3\t\n"," ultima linea sin line feed"]]
                "line 1 \n linea 2\nlinea 3\t\n ultima linea sin line feed"
                (deslinear ["line 1 \n", " linea 2\n", "linea 3\t\n"," ultima linea sin line feed"])
        ),
        (
            generarTest
                "Prueba para lineaSegura (1 de 2)"
                ["linea sin line feed"]
                "linea sin line feed\n"
                (lineaSegura "linea sin line feed")
        ),
        (
            generarTest
                "Prueba para lineaSegura (2 de 2)"
                ["linea con line feed\n"]
                "linea con line feed\n"
                (lineaSegura "linea con line feed\n")
        )
    ]

ejecutarPruebas = correrLasSiguientesPruebas tablasDePrueba 