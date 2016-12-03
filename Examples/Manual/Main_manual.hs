{-# LANGUAGE DataKinds #-}

module Main_manual where

import CLaSH.Prelude
import qualified Prelude as P
import Text.Printf
import Text.Show.Functions
import System.CPUTime
import Control.DeepSeq


import OffloadableFunctions_manual


-- testbench settings
lim = 10^4 :: Int

offloadBench :: Bool
offloadBench = False
 
 
-- testbench time function
time :: (Show b) => (a -> b) -> String -> [a] -> IO ()
time f name list = do
    putStrLn ("Starting benchmark for function: " P.++ (name))    
    start <- getCPUTime
    let x = P.map f list
    rnf (show x) `seq` return () -- Show introduces quite some overhead; Replace for serious testing!
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.9f sec (%d times)\n" (diff :: Double) lim
    printf "Individual time: %0.9f sec\n" (diff / fromIntegral lim :: Double)
    putStrLn ("Benchmark for function " P.++ name P.++ " completed.")
    return ()

-- main function
main = do
    
    -- benchmark for 32 bits multiplications
    let inpListmult = (\x -> P.zip x (P.repeat (2 :: Signed 32))) [(1 :: Signed 32)..(fromIntegral lim)]
    time mult "mult" inpListmult
    if offloadBench 
        then time multOffload "multOffload" inpListmult   
        else putStrLn "Skipping codesign benchmark as the offloadBench boolean is set to False."
       
    -- benchmark for matrix multiplication
    let matxs x = (((x :: Signed 16):>x:>x:>Nil):>(x:>x:>x:>Nil):>(x:>x:>x:>Nil):>Nil)
    let mat1s = (((1 :: Signed 16):>1:>1:>Nil):>(1:>1:>1:>Nil):>(1:>1:>1:>Nil):>Nil)
    let mat1to9 = (((1 :: Signed 16):>2:>3:>Nil):>(4:>5:>6:>Nil):>(7:>8:>9:>Nil):>Nil)
    
    let inpListmatrixMult =  [(1 :: Signed 16)..(fromIntegral lim)]
    time (\x -> matrixMult (matxs x,mat1s)) "multMatrix" inpListmatrixMult
    if offloadBench 
        then time (\x -> matrixMultOffload (matxs x,mat1s)) "multMatrixOffload" inpListmatrixMult   
        else putStrLn "Skipping codesign benchmark as the offloadBench boolean is set to False."       
       
    let inpListFir = [(1 :: Signed 32) ..(fromIntegral lim)]
    time (\x -> fir (repeat 0) x) "fir" inpListFir
    if offloadBench
        then time firOffload "firOffload" inpListFir
        else putStrLn "Skipping codesign benchmark as the offloadBench boolean is set to False."
    
   