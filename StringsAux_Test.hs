import StringsAux
import Tester


-- Tabla de pruebas (test)
tablasDePrueba = 
    [ 
        (
            generarTest 
                "dividirSimbolos" 
                ["asdf\r\n\thola "] 
                ["asdf","\r","\n","\t","hola"," "] 
                (dividirSimbolos "asdf\r\n\thola ")
        )
        ,
        (
            generarTest
                "unirSimbolos" 
                [ show ["asdf","\r","\n","\t","hola"," "]]
                "asdf\r\n\thola " 
                (unirSimbolos ["asdf","\r","\n","\t","hola"," "])
        )
    ] -- TODO: Agregar Pruebas

ejecutarPruebas = correrLasSiguientesPruebas tablasDePrueba 