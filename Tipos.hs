module Tipos where

class CCuerpo a where
  getCuerpo :: [ String ] -> Maybe ( [ a ] , [String])

type Nombre       = String

type Cuestionario a = [ Ejercicio a ]
type Cuerpo a       = [ a ]
data Ejercicio    a = Ejercicio Comentario Nombre (Cuerpo a) Comentario
  deriving Show
data Comentario     = COM [ String ]
  deriving Show

----------------------------------------
--- Tipos para la segunda parte

data QA           = Q Pregunta | A Respuesta
   deriving Show
type Pregunta     = [ Fragmento ]
data Respuesta    = ESSAY | MO [ Opcion ] | FV Bool | SHORT [ Opcion ] 
   deriving Show
data Opcion       = OK [ Fragmento ] | NOK [ Fragmento ]
   deriving Show
data Fragmento    = TXT String | MATH String | CODE String
   deriving Show

data Formato = LaTeX | HTML

