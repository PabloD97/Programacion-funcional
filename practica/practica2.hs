
apply f = g
   where g x = f x

first (x,y) = x

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

funcion f = g
  where g y x= f(x+y) 