{-# LANGUAGE DataKinds, ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=Offload.Plugin.OffloadPlugin #-} -- required for inserting Core plugin here in GHC

module OffloadableFunctions_auto where

import CLaSH.Prelude
import qualified Prelude as P

import Offload.Plugin.OffloadAnnotations -- required for annotations

import Offload.Haskell.OffloadFunctions
makeClassesInScope :: (BitPack Bit, KnownNat 0) => () -- required for the offloadplugin
makeClassesInScope = ()

-- -- =====================================
-- -- Offloadable function            
-- -- =====================================   

type SumType = Signed 64

{-# ANN sum4 OffloadPure #-}           
sum4 :: (Vec 4 (Signed 64)) -> (Signed 64)
sum4 inp      = outp
        where
                outp                     = fold (+) inp                 
                
{-# ANN matrixMult OffloadPure #-}                
matrixMult :: ((Vec 3 (Vec 3 (Signed 16))), (Vec 3 (Vec 3 (Signed 16)))) -> (Vec 3 (Vec 3 (Signed 16)))
matrixMult (a,b) = map (multLine (transpose b)) a
        where
                multLine c d = map (dotProduct d) c
                dotProduct e f = sum (zipWith (*) e f)
                
               
                
{-# ANN mac OffloadMealy #-}  
mac ::  (Signed 32) 
                -> (Signed 32, Signed 32)
                -> (Signed 32, Signed 32)              
mac s (i0, i1)          = (s', s')
        where
                s'      = s + (i0 * i1)                
                
        
        


