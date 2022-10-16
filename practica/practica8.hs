data N = Z | S N

evalN :: N -> Int
evalN Z = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z m = m
addN (S n) m S (addN n m)

prodN :: N -> N -> N
prodN Z  m =
prodN (S n) m = addN( prodN n m) m

int2N :: Int -> N 
int2N 0 = Z
int2N n = S (int2N (n-1))


--------------------------
data DigBin = O | I
type NBin = [DigBin]

-- f [] = ...
-- f (x:xs) = ... f xs 

succNB :: NBin -> NBin
succNB [] = [I]
succNB (O:xs) = I : xs
succNB (I:xs) = O : succNB xs


addNB :: NBin -> NBin -> NBin
-- En este caso conviebe recorrer ambos
addNB [] [] 			= []
addNB (b1:bs1) [] 		= b1:bs1
addNB [] (b2:bs2)  		= b2:bs2
-- En una demostracion, que yo este recorriendo ambas
-- no significa que tengo que hacer induccion sobre ambas.
-- Capaz con la primera, ya demuestro.
-- No existe la induccion en simulteano
addNB (b1:bs1) (b2:bs2) = 
	addB b1 b2 ( addNB bs1 bs2)

addB O O rs = O : rs
addB O I rs = I : rs
addB I O rs = I : rs
addB I I rs = O : succNB rs

evalNB :: NBin -> Int
evalNB [] = []
evalNB (x:xs) = evalB x + 2*(evalNB xs)


evalB O  = 0
evalB I = 1

-------------------


nb2n :: NBin -> N
nb2n [] = Z
nb2n (x:xs) = 
	addN (db2n x) (prodN (S (S Z)) nb2n xs)

db2n O = Z 
db2n I = S Z

-- https://youtu.be/xnRBafyS2Fk?t=2396
-- xQUE Z es igual a 1. S N es una seucesion de 1

n2nb :: N -> NBin
n2nb Z = []
n2nb (S n) = succNB (n2nb n)

-- Lo que hace sawady, sigue la regla de consumo de izq a der, que es lo que se lleva bien
-- con las demostraciones. 
normalizarNB :: NBin -> NBin
normalizarNB [] 	= []
normalizarNB (x:xs) = 
	normD x (normalizarNB xs) -- yo se que lo que esta aca (normalizarNB xs) no tiene 0 a la derecha

-- Siempre recordar el esquema 
-- f [] = ...
-- f (x:xs) = ... f xs

-- f [] = ...
-- f (x:xs) = subtarea x ... f xs
-- subtarea x = que hace algo con eso 

-- https://youtu.be/xnRBafyS2Fk?t=2773 que hermosa manera de pensarlo
normD O [] = []
normD b bs = b : bs

-- Version burda que hay de este ejercicio https://youtu.be/xnRBafyS2Fk?t=3051
-- subtarea O [] = []
-- subtarea O (b:bs) = O : b : bs
-- subtarea I [] = [I]
-- subtarea I (b:bs) = I : b : bs

-------------

-- Demostacion 
-- https://youtu.be/xnRBafyS2Fk?t=3145

-- evalNB . normalizarNB = evalNB

-- Por procipio de induccion estructural
-- Para todo xs
evalNB (normalizarNB xs) = evalNB xs

-- Voy a demsotrar por induccion sobre xs
-- Caso base xs = []
¿ evalNB (normalizarNB []) = evalNB [] ?

-- Caso base ind xs = (b:bs)
HI) evalNB (normalizarNB bs) = evalNB bs
TI) ¿ evalNB (normalizarNB (b:bs)) = evalNB (b:bs) ?

-- Lado izq
evalNB ( normalizarNB (b:bs))
=				-- def normalizarNB
evalNB (normD b (normalizarNB bs)) -- https://youtu.be/xnRBafyS2Fk?t=3333 
= -- Lema evalNb-normD, donde b = b, bs = normalizarNB bs
evalB b + 2 * (evalNB (normalizarNB bs))

-- Lado der 
evalNB (b:bs)
= 		-- def evalNB
evalB b + 2 * (evalNB bs)
= -- HI
evalB b + 2 * (evalNB (normalizarNB bs))
-- https://youtu.be/xnRBafyS2Fk?t=3427

-- https://youtu.be/xnRBafyS2Fk?t=3609
-- El hdp saco un lema para poder seguir
-- Lema evalNb-normD
evalNB (normD b bs)
=
evalB b + 2 * (evalNB bs)

-- Voy a demostrar por casos
Caso b = O, bs = []
¿evalNB (normD O []) = evalB O + 2 * (evalNB [])?

-- lado Izq
evalNB (normD O [])
=	-- DEF normD
evalNB []
= 	-- def evalNB
0

-- lado Der
evalB O + 2 * (evalNB [])
=	-- DEF evalNB
evalB O + 2 * 0
= 	-- arit.
evalB O 
= 	-- def evalB
0


Caso cualquier otro caso de b y bs
¿evalNB (normD b bs) = evalB b + 2 * (evalNB bs)?

-- lado Izq
evalNB (normD b bs)
=	-- DEF normD
evalNB (b:bs)
= 	-- def evalNB
evalB b + 2 * (evalNB bs)
-- lado Der


