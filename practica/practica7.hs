-- 1)
data Pizza = Prepizza | Capa Ingrediente Pizza  deriving (Eq, Show)
data Ingrediente = Aceitunas Int| Anchoas |Cebolla|Jamon | Queso |Salsa  deriving (Eq, Show)
 
-- 2) 
-- f Prepizza = 
-- f (Capa ing p) = ... f p ...
-- Si no sigue lo de arriba, esta mal demostrada la recursion estrutural.
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing p) = 1 + cantidadDeCapas p

-- Meter un if aca, es complicarla. Si lo metemos, cuando hagamos 
-- la demostracio, vamos a tener que hacerlo por casos.
-- A nivel codigo no es mala,pero a nivel demostracion si lo es.
cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa ing p) = aceitunas ing + cantidadDeAceitunas p

aceitunas :: Ingrediente -> Int
aceitunas (Aceitunas n) = n
aceitunas _				= 0

---------------

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza 		= Prepizza
duplicarAceitunas (Capa ing p)  =  
	Capa (aceitunasPorDos ing) (duplicarAceitunas p)

aceitunasPorDos :: Ingrediente -> Ingrediente
aceitunasPorDos (Aceitunas n) = Aceitunas (n*2)
aceitunasPorDos ing			  = ing
-- Igualito a la funcion 'id' esto. Cuando nos viene algo que no queremos
-- hacer nada, lo devolvemos como vino.

---------------
-- Como un filter
sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza 	= Prepizza
sinLactosa (Capa ing p) = 
	sacarQueso ing (sinLactosa p)

sacarQueso :: Ingrediente -> Pizza -> Pizza
sacarQueso Queso p = p
sacarQueso ing p   = Capa ing p

-- sacarQueso :: Ingrediente -> (Pizza -> Pizza)
-- sacarQueso Queso = id
-- sacarQueso ing   = Capa ing 
-- Esta es equivalente a la de arriba D:

--------------------
aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza = True
aptaIntolerantesLactosa (Capa ing p) =
	not (esQueso ing) && aptaIntolerantesLactosa p

-- Es bueno que las funciones siempre sean en afirmativo
esQueso :: Ingrediente -> Bool
esQueso Queso = False
esQueso _ = True

--------------------
-- normilizacion -> tengo pizzas similares y las convierto en una
conDescripcionMejorada :: Pizza -> Pizza 
conDescripcionMejorada Prepizza 	= Prepizza
conDescripcionMejorada (Capa ing p) = 
	comprimirAceitunas ing (conDescripcionMejorada p)

comprimirAceitunas :: Ingrediente -> Pizza -> Pizza
comprimirAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = 
	Capa (Aceitunas (n+m)) p
comprimirAceitunas ing p = Capa ing p
---------------------



-- SECCION II
type Nombre = String
data Planilla = Fin | Registro Nombre Planilla
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo

largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p 

esta :: Nombre -> Planilla -> Bool
esta nombre Fin = False
esta nombre (Registro n p) = nombre == n || esta p

juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin p2 = p2
juntarPlanillas (Registro n p) p2 = Registro n (juntarPlanillas p p2)

nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario n) = 0
nivelesJerarquicos (Investigador n e1 e2 e3) = 
	1 +
	nivelesJerarquicos e1 
	`max`
	nivelesJerarquicos e2
	`max`
	nivelesJerarquicos e3

cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n) = 1
cantidadDeIntegrantes (Investigador n e1 e2 e3) = 
	1 +
	cantidadDeIntegrantes e1 +
	cantidadDeIntegrantes e2 +
	cantidadDeIntegrantes e3

planillaDeIntegrantes :: Equipo -> Int
planillaDeIntegrantes (Becario n) = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = 
	Registro n 
	(
		planillaDeIntegrantes e1 
		`juntarPlanillas`
		planillaDeIntegrantes e2 
		`juntarPlanillas`
		planillaDeIntegrantes e3
	)

data Dungeon a = Habitacion a 
				| Pasaje (Maybe a) (Dungeon a)	
				| Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)


-- f (Habitacion a) 	= 
-- f (Pasaje m d)  	= ... f d
-- f (Bifurcacion m d1 d2) = ... f d1 ... f d2

cantidadDeBifurcaciones :: Dungeon -> Int
cantidadDeBifurcaciones (Habitacion a) 	= 0
cantidadDeBifurcaciones (Pasaje m d)  	= 0 + cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion m d1 d2) = 
	1 
	+ cantidadDeBifurcaciones d1 
	+ cantidadDeBifurcaciones d2

cantidadDePuntosInteresantes :: Dungeon -> Int
cantidadDePuntosInteresantes (Habitacion a) 	= 1
cantidadDePuntosInteresantes (Pasaje m d)  	= unoSiJust m  + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion m d1 d2) = 
	unoSiJust m
	+ cantidadDePuntosInteresantes d1 
	+ cantidadDePuntosInteresantes d2

-- case m of
--	Nothing -> cantidadDePuntosInteresantes d
-- 	Just x  -> 1 + cantidadDePuntosInteresantes d

unoSiJust :: Maybe -> Int
unoSiJust Nothing = 0
unoSiJust (Just x) = 1

cantidadDePuntosVacios :: Dungeon -> Int
cantidadDePuntosVacios (Habitacion a) 	= 1
cantidadDePuntosVacios (Pasaje m d)  	= unoSiEstaVacio m  + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d1 d2) = 
	unoSiEstaVacio m
	+ cantidadDePuntosVacios d1 
	+ cantidadDePuntosVacios d2

-- case m of
--	Nothing -> cantidadDePuntosInteresantes d
-- 	Just x  -> 1 + cantidadDePuntosInteresantes d

unoSiEstaVacio :: Maybe -> Int
unoSiEstaVacio Nothing = 1
unoSiEstaVacio (Just x) = 0


esLineal :: Dungeon -> Int
esLineal (Habitacion a) = True
esLineal (Pasaje m d)  	= esLineal d
esLineal (Bifurcacion m d1 d2) = False
	-- No tiene sentido hacer esto
	-- False && esLineal d1 && esLineal d2
	-- El 'False' absorve lo demas.

llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe e (Habitacion x)  	= e == x 
llenoDe e (Pasaje m d)  	= tieneE e m && llenoDe d
llenoDe e (Bifurcacion m d1 d2) = 
	tieneE e m 
	&& llenoDe e d1
	&& llenoDe e d2

tieneE e Nothing = False
tieneE e (Just x) = x == e
-- todo lo que esta del lado izquierdo de la flecha, se llama constraints
-- o condicion que tiene que cumplir un tipo.