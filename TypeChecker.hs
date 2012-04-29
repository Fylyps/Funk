module TypeChecker where

import AbsFunk
import ErrM
import Model
import FunkStdLib
import qualified Data.Map as Map  






checkTypes :: Prog -> Err Bool  
checkTypes (Program decs exp) = return True --fail "type error" 


