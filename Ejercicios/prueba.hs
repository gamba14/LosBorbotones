 
estaEn lista2 elemento = elem elemento lista2
intersection lista1 lista2 = any (estaEn lista2)lista1 
--devuelve bool
--intersection lista1 lista2 = filter (estaEn lista2)lista1 %devuelve lista 
f g x y = (g x,g y)
algo f g lista = filter f(map g lista)
esImpar lista = filter odd lista 
esMayoria a c = length(a c) > length(c)-length(a c)
vejez (_,edad)= edad > 60 
sonMuchos criterio listaPersonas = length(filter criterio listaPersonas) > length(listaPersonas)-length(filter criterio listaPersonas)
