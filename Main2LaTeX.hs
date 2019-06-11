
import Etapa1
import Etapa2
import Etapa3
import Data.Char (isSpace) 
import Salida
import Tipos

main =
  do putStr "Ingrese archivo de entrada : "
     entrada <- getLine
     txt     <- readFile $ entrada ++ ".giftX"
     case (getCuestionario (lines (dropWhile isSpace txt))) of
        Nothing -> putStrLn "Error en el cuestionario"
        Just c  -> putStrLn $ imprimirCuestionario LaTeX (transformaciones c)

