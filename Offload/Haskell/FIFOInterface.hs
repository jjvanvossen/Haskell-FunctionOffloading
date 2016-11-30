{-# OPTIONS_GHC -dynamic-too #-}

module Offload.Haskell.FIFOInterface 
(
        readFIFO,
        writeFIFO,
        readFIFO',
        writeFIFO',
)       
where

import qualified Data.ByteString.Lazy as B
import Data.Word (Word32)  
import System.IO (openBinaryFile, IOMode (ReadMode, WriteMode),hClose, Handle)
import Offload.Common.OffloadTypes (devReadFIFO, devWriteFIFO)

import System.IO.Error (catchIOError)
import GHC.IO.Exception (IOErrorType(..), ioe_type)

-- =================================                
-- Read from FIFO buffer through Xillyus IP
-- =================================
-- Read function used in non-pipelined function offloading
readFIFO :: Word32 -> IO B.ByteString
readFIFO len = do 
                fd <- openBinaryFile devReadFIFO ReadMode
                str <- readFIFO' fd len
                hClose fd
                return (str)

-- Read function that should be used in pipelined function offloading (i.e. no intermediate Handle/fd closing)
readFIFO' :: Handle -> Word32 -> IO B.ByteString
readFIFO' fd len = do
                str <- B.hGet fd (4 * (fromIntegral len)) -- reads in bytes so (4 * 32-bits word length)
                --print str -- uncomment to debug read result
                return (str)

-- =================================                
-- Write to FIFO buffer through Xillyus IP
-- =================================
-- Write function used in non-pipelined function offloading
writeFIFO :: B.ByteString -> IO Bool
writeFIFO str = do
                fd <- openBinaryFile devWriteFIFO WriteMode
                succes <- writeFIFO' fd str
                
                _ <- catchIOError  (hClose fd) (\e -> if catchInvalidArgument $ ioe_type e then return () else error $ show e) -- Catch the following (nasty?) occasional exception when closing the handle: /dev/xillybus_write_32: hClose: invalid argument (Bad file descriptor). It still closes the handle though :-D
                return succes
                        where
                                catchInvalidArgument :: IOErrorType   -> Bool
                                catchInvalidArgument InvalidArgument  = True
                                catchInvalidArgument _                = False
                
-- Write function that should be used in pipelined function offloading (i.e. no intermediate Handle/fd closing)
writeFIFO' :: Handle -> B.ByteString -> IO Bool
writeFIFO' fd str = do
                B.hPut fd str
                --print str -- uncomment to debug written argument
                return True 
