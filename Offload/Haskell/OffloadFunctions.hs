{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}

module Offload.Haskell.OffloadFunctions 
(
        offloadTemplate,
        offloadWriteTemplate,
        offloadReadTemplate
)
where

import CLaSH.Prelude
import qualified Prelude as P
import qualified Data.ByteString.Lazy as B
import GHC.TypeLits (KnownNat, Nat, type (*), type (+))
import Data.Word (Word8, Word32)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (Handle)
import Data.Binary (encode)

import Offload.Haskell.FIFOInterface
import Offload.Common.OffloadTypes

-- ==========================
-- Exported functions
-- ==========================


--write to offloaded function with already opened file descriptor             
offloadWriteTemplate    :: (WordPackConstraint tA nA remA, Integral c) 
                        => c 
                        -> (SNat nA, SNat remA) 
                        -> Handle
                        -> tA 
                        -> Bool
offloadWriteTemplate fnID (argWordLen, argRemBits) fdWrite argData = writeSuccess
    where   
        argWordLen'     = fromIntegral $ snatToInteger argWordLen  
        argSerData      = toList $ wordPack (argWordLen, argRemBits) argData
        argMessage      = applyProtocol (fromIntegral fnID) argWordLen' argSerData
        writeSuccess    = unsafePerformIO $! writeFIFO' fdWrite argMessage
  
             
             
-- read from offloaded function with already opened file descriptor
offloadReadTemplate     :: (WordPackConstraint tB nB remB, Integral c) 
                        => c 
                        -> (SNat nB, SNat remB) 
                        -> Handle
                        -> tB
offloadReadTemplate fnID (resWordLen, resRemBits) fdRead = resData
    where
        resWordLen'     = fromIntegral $ snatToInteger resWordLen
        resMessage      = unsafePerformIO $! readFIFO' fdRead $ fromIntegral (resWordLen' + 3)
        resSerData      = removeProtocol (fromIntegral fnID) resWordLen'  resMessage
        resData         = wordUnpack (resWordLen, resRemBits) $ toVector resSerData    
        
        
        
-- write & read to/from offloaded function
offloadTemplate         :: (WordPackConstraint tA nA remA, WordPackConstraint tB nB remB, Integral c) 
                        => c 
                        -> (SNat nA, SNat remA, SNat nB, SNat remB) 
                        -> tA 
                        -> tB
offloadTemplate fnID (argWordLen, argRemBits, resWordLen, resRemBits) argData = resData
    where
        -- writing argument
        argWordLen'     = fromIntegral $ snatToInteger argWordLen  
        argSerData      = toList $ wordPack (argWordLen, argRemBits) argData
        argMessage      = applyProtocol (fromIntegral fnID) argWordLen' argSerData
        writeSuccess    = unsafePerformIO $! writeFIFO argMessage
        
        -- reading result
        resWordLen'     = fromIntegral $ snatToInteger resWordLen
        resMessage      = unsafePerformIO $! readFIFO $ fromIntegral (resWordLen' + 3)
        resSerData      = removeProtocol (fromIntegral fnID) resWordLen'  resMessage        
        resData | writeSuccess == True  = wordUnpack (resWordLen, resRemBits) $ toVector resSerData 
                | otherwise             = error "Error whilst reading from fifo."        

-- ==========================
-- Serialization functions
-- ==========================             
-- Serialize data             
wordPack        :: forall tA nA remA. (WordPackConstraint tA nA remA)
                => (SNat nA, SNat remA)
                -> tA
                -> Vec (nA * 4) Word8
wordPack (wordSize, bitRem) inp = unpack (resize $ pack inp :: BitVector (nA * 32)) :: Vec (nA * 4) Word8 

-- Deserialize data
wordUnpack      :: forall tB nB remB. (WordPackConstraint tB nB remB)
                => (SNat nB, SNat remB)
                -> Vec (nB * 4) Word8
                -> tB
wordUnpack (wordSize, bitRem) inp = unpack $ resize (pack inp :: BitVector (nB * 32)) 
  

-- ==========================
-- Protocol functions
-- ==========================
-- Remove protocol from serialized data
removeProtocol  :: Word32 
                -> Word32
                -> B.ByteString -- length == (4 * (nB + 3))
                -> [Word8] -- length == (4 * nB)    
removeProtocol fnID len resultMessage = resultData
   where
        -- create correct header for comparison
        correctHeader = headerToBytes (beginFlag,fnID,len)
        -- split data packet in header and data                
        (rheader,rdata) = B.splitAt 12 resultMessage  
        -- verify protocol and return result data        
        resultData      | rheader P.== correctHeader = B.unpack rdata
                        | otherwise = error ("Protocol of received message was incorrect!" P.++ show rdata)
                                        
-- Apply protocol to serialized data
applyProtocol   :: Word32
                -> Word32 
                -> [Word8] -- length == (4 * nA)
                -> B.ByteString -- lenght == (4* (nA + 3))
applyProtocol fnID len argData = argMessage
    where                       
        --create protocol header 
        protocolheader = headerToBytes (beginFlag,fnID,len)
        -- append header to message
        argMessage = B.append protocolheader (B.pack argData)

-- ==========================
-- Other functions
-- ========================== 
                     
-- BitPack instance for Word8             
instance BitPack Word8 where
        type BitSize Word8 = 8
        pack v = pack (fromIntegral v :: Unsigned 8)
        unpack v = fromIntegral (unpack v :: Unsigned 8) :: Word8            
        
-- local version for list to vector conversions
toVector :: forall n a . KnownNat n =>  [a] -> Vec n a
toVector xs
    | xLen == nLen  = snd $ mapAccumL (\(y:ys) _ -> (ys, y)) xs ns
    | otherwise     = error errStr
    where
        ns          = repeat undefined :: Vec n a
        (xLen,nLen) = (P.length xs, length ns)
        errStr      = P.concat ["Not enough items recieved; expected ", show nLen, ", got ", show xLen]     
        
-- convert the protocol header to bytestring                                   
headerToBytes :: (Word32,Word32,Word32) -> B.ByteString
headerToBytes = encode 
        
