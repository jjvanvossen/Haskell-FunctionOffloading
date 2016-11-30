{-# LANGUAGE ScopedTypeVariables #-}
module Offload.Common.CLaSHCoercions where

import CLaSH.Prelude
import qualified Prelude as P

-- toList is included in CLaSH.Prelude
      
toVector :: forall n a . KnownNat n =>  [a] -> Vec n a
toVector xs
    | xLen == nLen  = snd $ mapAccumL (\(y:ys) _ -> (ys, y)) xs ns
    | otherwise     = error errStr
    where
        ns          = repeat undefined :: Vec n a
        (xLen,nLen) = (P.length xs, length ns)
        errStr      = P.concat ["Not enough items recieved; expected ", show nLen, ", got ", show xLen]      
               