{-# LANGUAGE DataKinds #-}

module Main_PipedWrite where

import CLaSH.Prelude
import qualified Prelude as P
import Text.Printf
import Text.Show.Functions
import System.CPUTime
import Control.DeepSeq

import OffloadableFunctions_manual
import Offload.Common.OffloadTypes 

import System.IO (hClose, hFlush)


-- testbench settings
lim = 10^4 :: Int

offloadBench :: Bool
offloadBench = True
 
 
-- testbench time function
time :: (Show b) => (a -> b) -> String -> [a] -> IO ()
time f name list = do
    putStrLn ("Starting benchmark for function: " P.++ (name))    
    start <- getCPUTime
    let x = P.map f list
    rnf (show x) `seq` return ()
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.9f sec (%d times)\n" (diff :: Double) lim
    printf "Individual time: %0.9f sec\n" (diff / fromIntegral lim :: Double)
    putStrLn ("Benchmark for function " P.++ name P.++ " completed.")
    return ()

-- main function
main = do       
    writeHandle <- fdWrite 
    
    -- mult benchmark
    let inpListmult = (\x -> P.zip x (P.repeat (2 :: Signed 32))) [(1 :: Signed 32)..(fromIntegral lim)]
    time mult "mult" inpListmult
    if offloadBench 
        then time (multOffloadWrite writeHandle) "multOffloadWrite" inpListmult   
        else putStrLn "Skipping codesign benchmark as the offloadBench boolean is set to False."
       
    -- matrix multiplication benchmark
    let matxs x = (((x :: Signed 16):>x:>x:>Nil):>(x:>x:>x:>Nil):>(x:>x:>x:>Nil):>Nil)
    let mat1s = (((1 :: Signed 16):>1:>1:>Nil):>(1:>1:>1:>Nil):>(1:>1:>1:>Nil):>Nil)
    let mat1to9 = (((1 :: Signed 16):>2:>3:>Nil):>(4:>5:>6:>Nil):>(7:>8:>9:>Nil):>Nil)
    
    let inpListmatrixMult =  [(1 :: Signed 16)..(fromIntegral lim)]
    time (\x -> matrixMult (matxs x,mat1s)) "multMatrix" inpListmatrixMult
    if offloadBench 
        then time (\x -> (matrixMultOffloadWrite writeHandle) (matxs x,mat1s)) "matrixMultOffloadWrite" inpListmatrixMult   
        else putStrLn "Skipping codesign benchmark as the offloadBench boolean is set to False." 
        
    -- fir benchmark
    let inpListFir = [(1 :: Signed 32) ..(fromIntegral lim)]
    time (\x -> fir (repeat 0) x) "fir" inpListFir
    if offloadBench
        then time (firOffloadWrite writeHandle) "firOffloadRead" inpListFir
        else putStrLn "Skipping codesign benchmark as the offloadBench boolean is set to False."
        
        
        
    hFlush writeHandle
    -- halt program to keep Handle opened
    print "Enter any character to end the program (Which will close the Handle)."
    _ <- getLine
    hClose writeHandle
    return ()