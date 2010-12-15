
module TupleProjection_TH (tuple_instances) where
  -- the top-level tuple_instances function needs to be defined in a separate module because of GHC's stage restriction

import Language.Haskell.TH
import Control.Monad
import MyUtil
import Prelude hiding ((.))

tupelize :: [Type] -> Type
tupelize [x] = x
tupelize l = foldr (flip AppT) (TupleT $ length l) (reverse l)

tuple_instances :: Int -> Q [Dec]
tuple_instances max_projectable_tuple_size =
  forM [2 .. max_projectable_tuple_size] $ \n -> do
    type_vars@(tv0 : tvr@(tv1 : _)) <- (VarT .) . replicateM n (newName "y")
    var_names <- replicateM n (newName "x")
    let ve0 : vt@(ve1 : _) = VarE . var_names
    return $ InstanceD [{- context -}]
      (ConT (mkName "Tuple") `AppT` tupelize type_vars `AppT` tv0 `AppT` tv1 `AppT` tupelize tvr)
      [FunD (mkName "chop_tuple")
        [Clause [TupP $ VarP . var_names] (NormalB $ ConE (mkName "CT") `AppE` ve0 `AppE` ve1 `AppE` TupE vt) [{- where decls -}]]]

{- tuple_instances generates instances of the form:

instance Tuple (a, b) a b b where chop_tuple (a, b) = CT a b b
instance Tuple (a, b, c) a b (b, c) where chop_tuple (a, b, c) = CT a b (b, c)
instance Tuple (a, b, c, d) a b (b, c, d) where chop_tuple (a, b, c, d) = CT a b (b, c, d)
instance Tuple (a, b, c, d, e) a b (b, c, d, e) where chop_tuple (a, b, c, d, e) = CT a b (b, c, d, e)
-}

