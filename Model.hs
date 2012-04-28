module Model where

import AbsFunk
import Data.Map
import Data.List


data Value = Vint Integer | VList [Value] | Vfun {f :: Value -> Value}
instance Show Value where
	show (Vint i) = show i
	show (Vfun _) = "<fun>"
	show (VList t) = "[" ++ intercalate "," (Prelude.map show t) ++ "]"

type Env = Map Ident Value


