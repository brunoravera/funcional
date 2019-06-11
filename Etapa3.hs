module Etapa3 where

import Tipos
import Data.Char (isSpace)

mo2short  :: Cuestionario QA -> Cuestionario QA
mo2short = undefined

sortMO    :: Cuestionario QA -> Cuestionario QA
sortMO = undefined

trim      :: Cuestionario QA -> Cuestionario QA
trim = undefined

nodupMO   :: Cuestionario QA -> Cuestionario QA
nodupMO = undefined

filtroFV  :: Cuestionario QA -> Cuestionario QA
filtroFV = undefined

transformaciones = mo2short . sortMO . nodupMO . filtroFV . trim
