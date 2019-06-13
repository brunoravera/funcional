
module Etapa1 where 
import Data.Char (isSpace)
import Tipos


--- El programa leeMX lee una lista hasta encontrar una marca. Si no encuentra
--- marcas, devuelve Nothing.

leeMX :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
leeMX m [] = Nothing
leeMX m (x:xs)
   | m x       = Just ([ ],  x, xs)
   | otherwise = do (ws, z, zs) <- leeMX m xs
                    return (x:ws, z, zs)

--- El programa leeMO lee una lista hasta encontrar una marca. Si no encuentra
--- marcas, devuelve Nothing en el segundo argumento y la lista vacía en el
--- tercero.

leeMO :: (a -> Bool) -> [a] -> ([a], Maybe a, [a])
leeMO m [] = ([], Nothing, [])
leeMO m (x:xs)
 | m x = ([], Just x, xs)
 | otherwise = case (leeMO m xs) of
             ([], Nothing, []) -> (x:xs, Nothing, [])
             (ws, Nothing, []) -> (x:ws, Nothing, [])
             (ws, Just z, zs) -> (x:ws, Just z, zs)



getCuestionario :: (CCuerpo a) => [ String ] -> Maybe (Cuestionario a)
getCuestionario txt =
   do (ys, zs) <- getEjercicios txt
      return ys

getEjercicio :: (CCuerpo a) => [ String ] -> Maybe (Ejercicio a, [ String ])
getEjercicio xs =
  do (c1, ys)  <- getComs   xs
     (nm ,ws)  <- getNombre ys
     (qas, zs) <- getCuerpo ws
     (c2 , tl) <- getComs   zs
     return (Ejercicio c1 nm qas c2, skipNLs tl)


--- Este analizador identifica una secuencia de Ejercicio. Para ello
--- debe usar el analizador anterior getEjercicio.
--- Identifica todos los ejercicios que pueda

getEjercicios :: (CCuerpo a) => [ String ] -> Maybe ([Ejercicio a], [ String ])
getEjercicios    []  = Just ([], [])
getEjercicios xss = 
  do (a, ys) <- getEjercicio xss
     (bs, ws) <- getEjercicios ys
     return (a:bs , ws)

--- Predicado; decide si una línea es un comentario
esComentario :: String -> Bool
esComentario xs = take 2 xs == "//"


skipNLs :: [ String ] -> [ String ]
skipNLs = dropWhile (all isSpace)

--- Un comentario es una línea de la forma "//..."
--- getComs lee comentarios sucesivos. Devuelve Nothing
--- si no hay comentarios al inicio.
--- El inicio del comentario está dado por dos "//",
--- que se eliminan. Si hubieran más "/" al comienzo de la línea,
--- los "/" adicionales se reemplazan por blancos " ".

--aca deberiamos iterar hasta tener algo que no comience con //, y traerlos todos juntos como el mismo COM
getComs :: [ String ] -> Maybe (Comentario, [ String ])
getComs xs
 | getComsAux xs == [] = Nothing
 | otherwise = Just((COM (getComsAux xs), getNotComsAux xs))

getComsAux :: [String] -> [String]
getComsAux [] = []
getComsAux (x:xs)
 | esComentario x = addSpacesComs (drop 2 x) : getComsAux xs
 | otherwise = []

getNotComsAux :: [String] -> [String]
getNotComsAux [] = []
getNotComsAux(x:xs)
 | esComentario x = getNotComsAux xs
 | otherwise = x:xs

addSpacesComs :: String -> String
addSpacesComs x
 | take 1 x == "/" = " " ++ addSpacesComs (drop 1 x)
 | otherwise = x

--- Un nombre está parentizado por "::".
--- Debe empezar y terminar en una unica linea, y estar al comienzo.
--- getNombre lee un nombre separando la línea en dos. Devuelve Nothing
--- si no hay nombre al inicio.

getNombre ((':' : ':' : nmqas) : ys)
  = let (nm , qas) = break (== ':') nmqas
        f (w : ws) = if (all isSpace w) then ws else (w:ws)
    in case qas of
         _:':':zs   -> Just (nm, f (zs:ys))
         otherwise -> Nothing
getNombre _
  = Nothing


instance CCuerpo Char where
   getCuerpo xs = do (qas, z, zs) <- leeMX esComentario xs
                     return (unlines qas, z:zs)