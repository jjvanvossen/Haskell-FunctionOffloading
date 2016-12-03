{-# LANGUAGE DataKinds #-}

module Main_auto where

import OffloadableFunctions_auto
import CLaSH.Prelude
import qualified Prelude as P

main = do
        print $ sum4 (1:>2:>3:>4:>Nil)
        
        let mat1s = (((1 :: Signed 16):>1:>1:>Nil):>(1:>1:>1:>Nil):>(1:>1:>1:>Nil):>Nil)
        let mat1to9 = (((1 :: Signed 16):>2:>3:>Nil):>(4:>5:>6:>Nil):>(7:>8:>9:>Nil):>Nil)
        print $ matrixMult (mat1s,mat1to9)
        
        print $ mac (0) (10,2)
    
   