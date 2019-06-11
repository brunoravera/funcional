module Salida where

import Data.Char (isSpace) 
import Tipos

-----------
--- SALIDA

mostrarCuestionario :: Show a => Cuestionario a -> String
mostrarCuestionario qs =
  foldl (\b a -> b ++ (mostrarEjercicio a)) "CUESTIONARIO\n============\n\n" qs

mostrarEjercicio :: Show a => Ejercicio a -> String
mostrarEjercicio (Ejercicio c1 nm qas c2) =
  "PREGUNTA " ++ nm ++ "\n" ++
  mostrarComentario "COMENTARIO INICIAL: " c1 ++
  show qas ++ "\n" ++
  mostrarComentario "COMENTARIO FINAL: " c2 ++
  "------------\n"

mostrarComentario :: String -> Comentario -> String
mostrarComentario h (COM coms) = h ++ unlines coms

--------------------

imprimirCuestionario :: Formato -> Cuestionario QA -> String
imprimirCuestionario fmt q =
   imprimirInicio fmt
   ++ unlines (map (imprimirEjercicio fmt) q)
   ++ imprimirFinal fmt

imprimirEjercicio :: Formato -> Ejercicio QA -> String
imprimirEjercicio fmt (Ejercicio c1 nm qa c2) =
   imprimirComentario fmt c1
   ++ imprimirNombre fmt nm
   ++ imprimirCuerpo fmt qa
   ++ imprimirComentario fmt c2

imprimirComentario :: Formato -> Comentario -> String
imprimirComentario LaTeX (COM com) = entornoLaTeX "giftComentario" (unlines com)
imprimirComentario HTML (COM com) = entornoHTML "giftComentario" (unlines com) 
imprimirNombre :: Formato -> String -> String
imprimirNombre LaTeX nm = comandoLaTeX "giftNombre" nm
imprimirNombre HTML  nm = comandoHTML "giftNombre" nm

imprimirInicio :: Formato -> String
imprimirFinal :: Formato -> String

imprimirInicio LaTeX =
  "\\documentclass{gift}\n" ++
  "\\begin{document}\n"
imprimirInicio HTML =
  "<html>\n" ++
  "\t<head>\n" ++
  "\t\t<link rel=\"stylesheet\" href=\"gift.css\">\n" ++
  "\t</head>\n" ++
  "\t<body>\n"

imprimirFinal LaTeX = "\\end{document}\n"
imprimirFinal HTML = "\t</body>\n</html>\n"

imprimirCuerpo :: Formato -> Cuerpo QA -> String
imprimirCuerpo fmt = unlines . map (imprimirQA fmt)

imprimirQA :: Formato -> QA -> String
imprimirQA fmt (Q fragmentos) = unlines $ map (imprimirFragmento fmt) fragmentos
imprimirQA LaTeX (A ESSAY) = comandoLaTeX "giftEssay" ""
imprimirQA LaTeX (A (FV b)) = comandoLaTeX "giftFV" (show b)
imprimirQA LaTeX (A (MO lopc)) = entornoLaTeX "giftMO" (imprimirMO LaTeX lopc)
imprimirQA LaTeX (A (SHORT lopc)) = entornoLaTeX "giftShort" (imprimirShort LaTeX lopc)
imprimirQA HTML (A ESSAY) = comandoHTML "giftEssay" ""
imprimirQA HTML (A (FV b)) = comandoHTML "giftFV" (show b)
imprimirQA HTML (A (MO lopc)) = entornoHTML "giftMO" (imprimirMO HTML lopc)
imprimirQA HTML (A (SHORT lopc)) = entornoHTML "giftShort" (imprimirShort HTML lopc)


imprimirMO :: Formato ->  [ Opcion ] -> String
imprimirShort :: Formato ->  [ Opcion ] -> String
imprimirMO    LaTeX = unlines . map (\opc -> "\\item " ++ imprimirOpc LaTeX opc)
imprimirMO    HTML = unlines . map (\opc -> "\\item " ++ imprimirOpc HTML opc)
imprimirShort LaTeX = unlines . map (\opc -> "\\item " ++ imprimirOpc LaTeX opc)
imprimirShort HTML = unlines . map (\opc -> "\\item " ++ imprimirOpc HTML opc)

imprimirOpc :: Formato -> Opcion -> String
imprimirOpc   LaTeX (OK fragmentos) = unlines $ map (imprimirFragmento LaTeX) fragmentos
imprimirOpc   LaTeX (NOK fragmentos) = unlines $ map (imprimirFragmento LaTeX) fragmentos
imprimirOpc   HTML (OK fragmentos) = unlines $ map (imprimirFragmento HTML) fragmentos
imprimirOpc   HTML (NOK fragmentos) = unlines $ map (imprimirFragmento HTML) fragmentos

imprimirFragmento :: Formato -> Fragmento -> String
imprimirFragmento LaTeX (TXT str) = entornoLaTeX "giftFragmento" str
imprimirFragmento LaTeX (MATH str) = "$" ++ str ++ "$"
imprimirFragmento LaTeX (CODE str) = "\\verb!" ++ str ++ "!"
imprimirFragmento HTML (TXT str) = entornoHTML "giftFragmento" str
imprimirFragmento HTML (MATH str) = comandoHTML "giftMATH" str
imprimirFragmento HTML (CODE str) = comandoHTML "giftCode" str

startLaTeXCom com = "\\" ++ com ++ "{"
endLaTeXCom = "}\n"
comandoLaTeX com str = startLaTeXCom com ++ str ++ endLaTeXCom
startLaTeXEnv env = "\\begin{" ++ env ++ "}\n"
endLaTeXEnv   env = "\\end  {" ++ env ++ "}\n"
entornoLaTeX env str = startLaTeXEnv env ++ str ++ endLaTeXEnv env

startHTMLspan com = "<span class=\"" ++ com ++ "\">\n"
endHTMLspan = "</span>\n"
comandoHTML com str = startHTMLspan com ++ str ++ endHTMLspan
startHTMLdiv com = "<div class=\"" ++ com ++ "\">\n"
endHTMLdiv = "</div>\n"
entornoHTML com str = startHTMLdiv com ++ str ++ endHTMLdiv

