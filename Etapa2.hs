module Etapa2 where

import Data.Char (isSpace)
import Tipos
import Etapa1

--- El programa leeMOE lee una lista hasta encontrar una marca que no
--- esté protegida por un ESCAPE. Si no encuentra
--- marcas, devuelve Nothing en el segundo argumento y la lista vacía en el
--- tercero. Si el ESCAPE es el ultimo elemento, devuelve Nothing.

leeMOE :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe ([a], Maybe a, [a])
leeMOE m e [] = Just ([], Nothing, [])
leeMOE m e [x]
   | e x       = Nothing
   | m x       = Just ([ ],  Just x, [])
   | otherwise = Just ([x], Nothing, [])
leeMOE m e (x:y:xs) = auxMOE m e False (x:y:xs) [] 


auxMOE :: (a -> Bool) -> (a -> Bool) -> Bool -> [a] -> [a]-> Maybe ([a], Maybe a, [a])
auxMOE m e a [] ls = Just (ls, Nothing, [])
auxMOE m e a [x] ls
 | e x = Nothing
auxMOE m e a (x:xs) ls
 | not a = if m x then Just(ls, Just x, xs) else (auxMOE m e (e x) xs (ls++[x]))
 | otherwise = (auxMOE m e (e x) xs (ls++[x]))

--- El programa leeMXE lee una lista hasta encontrar una marca que no
--- esté protegida por un ESCAPE. Si no encuentra
--- marcas, devuelve Nothing. Si el ESCAPE es el ultimo elemento,
--- devuelve Nothing.

leeMXE :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe ([a], a, [a])
leeMXE m e xs = auxMXE m e False xs []

auxMXE :: (a -> Bool) -> (a -> Bool) -> Bool -> [a] -> [a]-> Maybe ([a], a, [a])
auxMXE m e a [] ls = Nothing
auxMXE m e a [x] ls
 | e x = Nothing
auxMXE m e a (x:xs) ls
 | not a = if m x then Just(ls, x, xs) else (auxMXE m e (e x) xs (ls++[x]))
 | otherwise = (auxMXE m e (e x) xs (ls++[x]))

--- El programa readFV recibe un String, y consume del mismo
--- una palabra que representa un valor booleano de acuerdo
--- a la gramatica de GIFT*.

readFV :: String -> Maybe (Bool, String)
readFV str
  | (take 9 str `elem` wTrue && take 10 str `elem` wTrue) || (take 9 str `elem` wTrue && str !! 9 == ' ') = Just(True, drop 9 str)
  | (take 6 str `elem` wTrue && take 7 str `elem` wTrue) || (take 6 str `elem` wTrue && str !! 6 == ' ') = Just(True, drop 6 str)
  | (take 1 str `elem` wTrue && take 2 str `elem` wTrue) || (take 1 str `elem` wTrue && str !! 1 == ' ') = Just(True, drop 1 str)
  | (take 4 str `elem` wTrue && take 5 str `elem` wTrue) || (take 4 str `elem` wTrue && str !! 4 == ' ') = Just(True, drop 4 str)
  | (take 5 str `elem` wFalse && take 6 str `elem` wFalse) || (take 5 str `elem` wFalse && str !! 5 == ' ') = Just(False, drop 5 str)
  | (take 1 str `elem` wFalse && take 2 str `elem` wFalse) || (take 1 str `elem` wFalse && str !! 1 == ' ') = Just(False, drop 1 str)
  | otherwise = Nothing
  where wTrue  = [ "VERDADERO", "VERDAD", "V", "TRUE", "T" ]
        wFalse = [ "FALSO", "FALSE", "F" ]


getSeq :: ( a -> Maybe (b, a) ) -> a -> Maybe ([b], a)
getSeq f xs =
  case (f xs) of
       Nothing       -> Just ([], xs)
       Just (fx, zs) -> do (bs, a) <- getSeq f zs
                           return (fx:bs, a)



---- str2qas consume todo un string y lo convierte en [ QA ]
---- Devuelve Nothing si no se puede consumir toda la entrada.

str2qas :: String -> Maybe [ QA ]
str2qas xs = do (qs, zs) <- getSeq str2qa xs
                if (zs /= "") then Nothing else Just qs

---- Un QAs puede comenzar con una respuesta (marcada { ... }) o con
---- una pregunta (sin  marcas).


str2qa :: String -> Maybe (QA, String)
str2qa [    ]    = Nothing
str2qa (x:xs)
      | x == '{'     = do
       (a, b) <- str2a xs
       return ((A a), b)
      | x == '}' = do
        (a,b) <- str2q "\\n"
        return ((Q a), xs)
      | isSpace x      = str2qa xs
      | otherwise    = do
       (a, b) <- str2q (x:xs) 
       return ((Q a), b)


---- str2q procesa una Pregunta.

str2q :: String -> Maybe ( Pregunta  , String )
str2q = getSeq getFragmento

---- Analizador de Fragmento
---- No se aceptan caracteres especiales sin escape dentro de un fragmento.
---- Los mismos son: {, }, =, ~

getFragmento :: String -> Maybe (Fragmento, String)
getFragmento      ""  = Nothing
getFragmento ( x :xs)
    | x `elem` ['{', '}', '=', '~'] =  Nothing
    | x == '$'                      = str2Math    xs
    | x == '`'                      = str2Code    xs
    | otherwise                     = str2Txt     (x:xs)

--- Los MATH contienen cualquier caracter con escape que no sea $
str2Math :: String -> Maybe ( Fragmento , String )
str2Math str = do (math, _ , zs) <- leeMXE marca escape str
                  return ( MATH math , zs )
         where marca x = x == '$'

--- Los CODE contienen cualquier caracter con escape que no sea `
str2Code :: String -> Maybe ( Fragmento , String )
str2Code str = do (code, _, zs) <- leeMXE marca escape str
                  return ( CODE code , zs )
         where marca x = x == '`'

--- Los TXT en Q contienen cualquier caracter con escape que no sea ` $ {
str2Txt :: String -> Maybe ( Fragmento , String )
str2Txt str = do 
  (txt,Just m,zs) <- leeMOE marca escape str
  return (TXT txt, (m:zs))
        where marca x = x `elem` ['`','$','{','}','~','=']


---- str2a procesa una respuesta.

str2a :: String -> Maybe ( Respuesta , String )
str2a    ""  = Nothing
str2a xxs@(x:xs)
  | isSpace x      = str2a xs
  | x == '='       = do 
    (a, b) <- str2aAUX xxs
    return (MO a, b)
  | x == '~'       = do 
    (a, b) <- str2aAUX xxs
    return (MO a, b)
  | x == '}'       = Just (ESSAY , xs)
  | otherwise      = do (b, ys)     <- readFV xxs
                        return(FV b, ys)

str2aAUX :: String -> Maybe ( [Opcion] , String )
str2aAUX "" = Nothing
str2aAUX (x:xs)
  | isSpace x = str2aAUX xs
  | x == '=' = do 
    (a, b) <- getFragmento(xs)
    (z, w) <- str2aAUX (b)
    return ([(OK [a])] ++ z, w)
  | x == '~' = do 
    (a, b) <- getFragmento(xs)
    (z, w) <- str2aAUX (b)
    return ([(NOK [a])] ++ z, w)
  | x == '}'       = Just ([] , xs)
  | otherwise = Just ([] , xs)


leerOpcion :: String -> Maybe ( Opcion , String )
leerOpcion ('}':xs) = Nothing
leerOpcion ('=':xs) = do (op, zs) <- getSeq getFragmento xs
                         return (OK op , zs)
leerOpcion ('~':xs) = do (op, zs) <- getSeq getFragmento xs
                         return (NOK op , zs)

--- Un QA puede abarcar varias líneas. Termina en un comentario.
--- Se devuelve una única línea.

instance CCuerpo QA where
   getCuerpo xs = do (ys, zs) <- getCuerpo xs
                     qas <- str2qas (ys::Cuerpo Char)
                     return (qas, zs)

--- Marca para detectar el escape

escape = (== '\\')
