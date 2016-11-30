{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -dynamic-too #-}


module Offload.Common.OffloadTypes where

import Data.Word
import CLaSH.Prelude 
import GHC.TypeLits (KnownNat, type (*), type (+))
import Language.Haskell.TH
import System.IO (openBinaryFile, IOMode (ReadMode, WriteMode),hClose, Handle)

-- beginflag for datapacket protocol
beginFlag = 10 :: Word32

-- settings related to the SoCKit proof of concept
-- do not alter without changing xillybus IP
devReadFIFO  = "/dev/xillybus_read_32"
devWriteFIFO = "/dev/xillybus_write_32"
type FIFO_width = 32

fdRead = openBinaryFile devReadFIFO ReadMode
fdWrite = openBinaryFile devWriteFIFO WriteMode

-- =====================================
-- FPGA Wrapper Types            
-- =====================================
type Int32     = Signed 32
type XillybusType = Signed FIFO_width
type UInt32     = Unsigned 32

-- | constraint for defining bitpack and wordpack sizes
type BitPackConstraint t bits = (KnownNat bits, KnownNat (BitSize t), BitPack t, (BitSize t) ~ bits)
                  
type WordPackConstraint t n rem = (KnownNat n, KnownNat rem, KnownNat (BitSize t), BitPack t,  KnownNat (n * FIFO_width), (n * FIFO_width) ~ ((BitSize t) + rem), KnownNat (n * 4), ((n * 4) * 8) ~ (n * FIFO_width))

-- Combining the above constraints
type WordPackConstraints t a b c d = ( BitPackConstraint t a, WordPackConstraint t b d)  



-- =====================================
-- Shared functions
-- ===================================== 
-- Template Haskell function that is used to extract type level naturals from the given argument function at compile time.     
-- The below functions are modified variants of code copied from lambdaya-bus github repo: https://github.com/ra1u/lambdaya-bus/blob/master/src/System/RedPitaya/Bus/Tools.hs

-- This variant can extract the type level naturals of a pure (i.e. a -> b) typed function
wordPackPureNats' :: forall a a1 b b1.(BitPackConstraint a a1, BitPackConstraint b b1)
       => (a -> b) 
       -> Exp
wordPackPureNats' f =  TupE [snatT ap1,snatT ap2,snatT bp1,snatT bp2] 
        where
                snatT n = SigE (VarE 'snat ) (AppT (ConT ''SNat) (LitT (NumTyLit n)))
                
                fr n = (ra,rb) 
                        where
                                (a,b) = quotRem n 32
                                (ra,rb) | b == 0 = (a,0)
                                        | otherwise = (a+1,32-b)
                                        
                (ap1,ap2) = fr (snatToInteger (snat :: SNat a1))
                
                (bp1,bp2) = fr (snatToInteger (snat :: SNat b1))


-- | Template that helps deducing first argument of function 'busBuild'
wordPackPureNats :: forall a a1 b b1.(BitPackConstraint a a1, BitPackConstraint b b1)
       => (a -> b) 
       -> ExpQ
wordPackPureNats = return . wordPackPureNats'   


-- This variant can extract the type level naturals of a Mealy typed function
wordPackMealyNats' :: forall s a a1 b b1.(BitPackConstraint a a1, BitPackConstraint b b1)
       => (s -> a -> (s,b)) 
       -> Exp
wordPackMealyNats' f =  TupE [snatT ap1,snatT ap2,snatT bp1,snatT bp2] 
        where
                snatT n = SigE (VarE 'snat ) (AppT (ConT ''SNat) (LitT (NumTyLit n)))
                
                fr n = (ra,rb) 
                        where
                                (a,b) = quotRem n 32
                                (ra,rb) | b == 0 = (a,0)
                                        | otherwise = (a+1,32-b)
                                        
                (ap1,ap2) = fr (snatToInteger (snat :: SNat a1))
                
                (bp1,bp2) = fr (snatToInteger (snat :: SNat b1))


wordPackMealyNats :: forall s a a1 b b1.(BitPackConstraint a a1, BitPackConstraint b b1)
       => (s -> a -> (s,b)) 
       -> ExpQ
wordPackMealyNats = return . wordPackMealyNats'  


-- This variant can extract the type level naturals of a Moore typed function
wordPackMooreNats' :: forall s a a1 b b1.(BitPackConstraint a a1, BitPackConstraint b b1)
       => (s -> a -> s) 
       -> (s -> b)
       -> Exp
wordPackMooreNats' f1 f2 =  TupE [snatT ap1,snatT ap2,snatT bp1,snatT bp2] 
        where
                snatT n = SigE (VarE 'snat ) (AppT (ConT ''SNat) (LitT (NumTyLit n)))
                
                fr n = (ra,rb) 
                        where
                                (a,b) = quotRem n 32
                                (ra,rb) | b == 0 = (a,0)
                                        | otherwise = (a+1,32-b)
                                        
                (ap1,ap2) = fr (snatToInteger (snat :: SNat a1))
                
                (bp1,bp2) = fr (snatToInteger (snat :: SNat b1))


wordPackMooreNats :: forall s a a1 b b1.(BitPackConstraint a a1, BitPackConstraint b b1)
       => (s -> a -> s) 
       -> (s -> b)
       -> ExpQ
wordPackMooreNats f1 f2 = return $ wordPackMooreNats' f1 f2

