module SimpleArgvParser (pairArguments) where

import Prelude hiding (map)
import qualified Data.Map.Strict as Map

strIsOption :: String -> Bool
strIsOption (a:b:_) = (a == '-') && (b == '-')
strIsOption _ = False

-- TODO: use either here
pairArguments :: [String] -> Maybe (Map.Map String String)
pairArguments args = collect args Map.empty
  where
    collect [] map = Just map
    collect [_] _ = Nothing
    collect (k:v:rst) map = if strIsOption k then collect rst (Map.insert key v map) else Nothing
      where
        (_,key) = splitAt 2 k
