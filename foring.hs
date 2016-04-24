anioIngreo "george" = 2013
anioIngreso "sexy99" = 2012
anioIngreso "tirasteGasAbandonaste" = 2011
anioIngreso "tristelme" = 2010

maxPuntNiv "newbie" = 1
maxPuntNiv "intermedio" = 5
maxPuntNiv "experto" = 10

antiguedadUsuario usu = 2016 - anioIngreso usu

puntosBase usu = (length usu)*(antiguedadUsuario usu) 

nivelUsuario usu | antiguedadUsuario usu < 1 = "newbie"
                 | puntosBase usu < 50 = "intermedio"
                 | otherwise = "experto"

puedeOtorgar usu pts | (maxPuntNiv (nivelUsuario usu)) < pts = "no se puede"  
                     | otherwise = "otorgado!"   