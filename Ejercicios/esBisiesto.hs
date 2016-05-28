esMultiploDe n1 n2 = mod n1 n2 == 0
esBisiesto anio = esMultiploDe anio 400 || (esMultiploDe anio 4 && not(esMultiploDe anio 100))
cantidadDias anio | esBisiesto anio = "366"
				  | otherwise = "365"