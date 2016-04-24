pesoPino altura  =  (+)(min (altura*300) 900) 
                        (max ((altura-3)*200) 0)
esPesoUtil peso = (peso >= 400) && (peso <= 1000)
sirvePino = esPesoUtil.pesoPino