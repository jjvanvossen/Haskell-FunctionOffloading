{-# LANGUAGE DataKinds, ScopedTypeVariables, FlexibleContexts, TypeFamilies, TypeOperators #-}

module OffloadableFunctions_manual where

import CLaSH.Prelude
import qualified Prelude as P

import Offload.Haskell.OffloadFunctions
import System.IO (Handle)

--import Offload.Common.CLaSHCoercions

-- =====================================
-- simple multiplier          
-- =====================================   
mult    :: (Signed 32, Signed 32) 
        -> Signed 32
mult (ia, ib) =  (ia * ib)  

multOffload     :: (Signed 32, Signed 32) 
                -> Signed 32
multOffload = offloadTemplate 0 (d2,d0,d1,d0)  

multOffloadWrite :: Handle
                -> (Signed 32, Signed 32) 
                -> Bool
multOffloadWrite = offloadWriteTemplate 0 (d2,d0)

multOffloadRead  :: Handle 
                -> (Signed 32)
multOffloadRead = offloadReadTemplate 0 (d1, d0)        
            

-- =====================================
-- matrix multiplier          
-- ===================================== 
matrixMult      :: ((Vec 3 (Vec 3 (Signed 16))), (Vec 3 (Vec 3 (Signed 16)))) 
                -> (Vec 3 (Vec 3 (Signed 16)))
matrixMult (a,b) = map (multLine (transpose b)) a
        where
                multLine c d = map (dotProduct d) c
                dotProduct e f = sum (zipWith (*) e f)

matrixMultOffload       :: ((Vec 3 (Vec 3 (Signed 16))), (Vec 3 (Vec 3 (Signed 16)))) 
                        -> (Vec 3 (Vec 3 (Signed 16)))
matrixMultOffload  = offloadTemplate 1 (d9,d0,d5,d16)

matrixMultOffloadWrite :: Handle
                -> ((Vec 3 (Vec 3 (Signed 16))), (Vec 3 (Vec 3 (Signed 16))))
                -> Bool
matrixMultOffloadWrite = offloadWriteTemplate 1 (d9,d0)

matrixMultOffloadRead  :: Handle 
                -> (Vec 3 (Vec 3 (Signed 16)))
matrixMultOffloadRead = offloadReadTemplate 1 (d5,d16)     

-- =====================================
-- Finite-impulse response filter (from CLaSH website)     
-- ===================================== 

coeffs = (2:>3:>(-2):>8:>2:>3:>(-2):>8:>2:>3:>(-2):>8:>2:>3:>(-2):>8:>2:>3:>(-2):>8:>2:>3:>(-2):>8:>Nil) :: Vec 24 (Signed 32)     

fir :: Vec 24 (Signed 32) -> (Signed 32) -> (Vec 24 (Signed 32), (Signed 32))
fir xs inp                 = (xs', outp)
    where
        xs'                 = inp +>> xs
        ws                  = zipWith (*) coeffs xs'
        outp                = fold (+) ws              
        
firOffload      :: (Signed 32)
                -> (Signed 32)
firOffload      = offloadTemplate 2 (d1,d0,d1,d0) 

firOffloadWrite :: Handle
                -> (Signed 32) 
                -> Bool
firOffloadWrite = offloadWriteTemplate 2 (d1,d0)

firOffloadRead  :: Handle 
                -> (Signed 32)
firOffloadRead = offloadReadTemplate 2 (d1, d0)                

