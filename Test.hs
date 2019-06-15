import Data.Char (isSpace)

import Etapa1 hiding (getCuestionario)
import Etapa2
import Etapa3
import Salida
import Tipos

getCuestionario :: (CCuerpo a) => [ String ] -> Maybe (Cuestionario a)
getCuestionario txt =
   do (ys, zs) <- getEjercicios txt
      case zs of
        [] -> return ys
        _  -> Nothing


tests = ["t1","t2","t3"]
testsEtapa1 = map (\s -> "./test/Etapa1/" ++ s) tests
testsEtapa2 = map (\s -> "./test/Etapa2/" ++ s) tests
testsEtapa3 = map (\s -> "./test/Etapa3/" ++ s) (tests ++ ["t4","t5"])
testsRotos  = map (\s -> "./test/Rotos/" ++ s) (tests ++ ["t4","t5"])




main =
  do sequence_ [runTest testEtapa1 testNum | testNum <- testsEtapa1]
     sequence_ [runTest testEtapa2 testNum | testNum <- testsEtapa2]
     sequence_ [runTest testEtapa3 testNum | testNum <- testsEtapa3]
     sequence_ [runTest testEtapa3 testNum | testNum <- testsRotos]
     sequence_ [compile testNum | testNum <- testsEtapa3]




runTest testFunc nombreTest
  = do txt <- readFile (nombreTest ++ ".giftX")
       writeFile (nombreTest ++ ".sal")$ testFunc txt

testEtapa1 txt
  = case (getCuestionario (lines (dropWhile isSpace txt))) of
      Nothing -> "Error en el cuestionario"
      Just c  -> mostrarCuestionario (c :: Cuestionario Char)

testEtapa2 txt
  = case (getCuestionario (lines (dropWhile isSpace txt))) of
      Nothing -> "Error en el cuestionario"
      Just c  -> mostrarCuestionario (c :: Cuestionario QA)

testEtapa3 txt
  = case (getCuestionario (lines (dropWhile isSpace txt))) of
      Nothing -> "Error en el cuestionario"
      Just c  -> mostrarCuestionario $ transformaciones (c :: Cuestionario QA)


compile nombreTest
  = do txt <- readFile (nombreTest ++ ".giftX")
       case (getCuestionario (lines (dropWhile isSpace txt))) of
         -- llamar solo con cuestionarios bien formados
         Just c  -> do writeFile (nombreTest ++ ".html")
                        $ imprimirCuestionario HTML $transformaciones c
                       writeFile (nombreTest ++ ".tex")
                        $ imprimirCuestionario LaTeX $transformaciones c

