
apply f = g
   where g x = f x

first (x,y) = x

second (x, y) = y
swap (x,y) = (y,x)

uflip :: ((a,b) -> c) -> (b,a) -> c
uflip f = g
  where g p = f (swap p)

twice :: (a -> a) -> (a -> a)
twice f = g
  where g x = f (f x)

doble :: Int -> Int 
doble x = x + x

const' x = g 
  where g y = x

appDup f = g 
  where g x = f (x, x)

appFork (f, g) = h 
  where h x = (f x, g x)


appPar (f, g) = h 
  where h (x, y) = (f x, g y)


appDist f = g 
  where g (x, y) = (f x, f y)  

compose :: (b->c) -> (a->b) ->a -> c
compose f g x = f (g x) 

id x = x

flip f x y = (f y) x


data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon
data Helado = Vasito Gusto | Cucurucho Gusto Gusto |Pote Gusto Gusto Gusto

chocoHelate consH = consH Chocolate

data DigBin = O | I

dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

data Medida = Mm Float | Cm Float |  Inch Float | Foot Float deriving (Eq, Show)

asMm :: Medida -> Medida
asMm (Cm n) = Mm (n * 10)
asMm (Inch n) = Mm (n * 25.4)
asMm (Foot n) = Mm (n * 25.4)
asMm (Mm n) = Mm n

asCm :: Medida -> Medida 
asCm (Mm n) = Cm (n / 10)
asCm (Inch n) = Cm (n * 2.54)
asCm (Mm n) = Cm (n * 2.54)
asCm (Cm n) = Cm n

asInch :: Medida -> Medida
asInch (Inch n) = Inch n
asInch (Cm n) = Inch (n / 2.53)
asInch (Mm n) = Inch (n / 25.3)
asInch (Foot n) = Inch n

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f(x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f(x,y)= f x y


data Shape = Circle Float | Rect Float Float deriving (Eq, Show)

construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0

-- Ejercicio 7)
data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer | Other String
type ExHandler a = Exception -> a


-- definir la función
--tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b


-- Ejmeplo de uso:

-- sueldoGUIE :: Nombre -> [Empleado] -> GUI Int
-- sueldoGUIE nombre empleados =
--     tryCatch (lookupE nombre empleados) mostrarInt
--        (\e -> case e of
--                NotFound -> ventanaError msgNotEmployee
--                       _ -> error msgUnexpected)
--     where msgNotEmployee = "No es empleado de la empresa"
--     msgUnexpected = "Error inesperado"

-- sabiendo que:

-- mostrarInt :: Int -> GUI Int
-- ventanaError :: String -> GUI a
-- lookupE :: Nombre -> [Empleado] -> MayFail Int