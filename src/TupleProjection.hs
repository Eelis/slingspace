
module TupleProjection (project, project') where

import TupleProjection_TH
import Language.Haskell.TH

data ChoppedTuple hd s tl = CT { ct_hd :: hd, ct_s :: s, ct_tl :: tl }

class Tuple tup hd s tl | tup → hd, tup → s, tup → tl where chop_tuple :: tup → ChoppedTuple hd s tl

$(tuple_instances 5) -- this constant is completely arbitrary

project :: Integer → ExpQ
project 0 = [| ct_hd . chop_tuple |]
project 1 = [| ct_s . chop_tuple |]
project n = [| $(project (n - 1)) . ct_tl . chop_tuple |]

project' :: Int → Int → ExpQ
project' x y = newName "bla" >>= \n →
  return $ LamE [TupP (replicate y WildP ++ [VarP n] ++ replicate (x - y - 1) WildP)] (VarE n)

-- project' differs from project in that it takes an additional initial parameter specifying the tuple size

