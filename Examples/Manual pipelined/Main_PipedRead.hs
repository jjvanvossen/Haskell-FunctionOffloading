{-# LANGUAGE DataKinds #-}

module Main_PipedRead where

import CLaSH.Prelude
import qualified Prelude as P
import Text.Printf
import Text.Show.Functions
import System.CPUTime
import Control.DeepSeq

import OffloadableFunctions_manual
import Offload.Common.OffloadTypes 

import System.IO (hClose)

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
       
    let readCounter = [(1 :: Signed 32) ..(fromIntegral lim)]
    readHandle <- fdRead
    if offloadBench
        then time (\x -> show $! multOffloadRead readHandle) "multOffloadRead" readCounter
        else putStrLn "Skipping codesign benchmark as the offloadBench boolean is set to False."
    if offloadBench
        then time (\x -> show $! matrixMultOffloadRead readHandle) "matrixMultOffloadRead" readCounter
        else putStrLn "Skipping codesign benchmark as the offloadBench boolean is set to False."
    if offloadBench
        then time (\x -> show $! firOffloadRead readHandle) "firOffloadRead" readCounter
        else putStrLn "Skipping codesign benchmark as the offloadBench boolean is set to False."
    hClose readHandle
    return ()