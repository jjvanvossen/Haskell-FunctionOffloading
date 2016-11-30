-- This module may be used to test the CLaSH design.


{-# LANGUAGE ScopedTypeVariables #-}

module TestTopEntity where

import CLaSH.Prelude

import Offload.Common.OffloadTypes
import CLaSHTopEntityModule_manual
import qualified Prelude as P


import CLaSH.Prelude.DataFlow
import CLaSH.Prelude.Mealy    (mealyB')
import CLaSH.Signal.Explicit  (Clock (..), Signal', SystemClock, sclock)

-- =============================
-- new fifoDF implementation which contains 0's instead of undefined in empty elements, i.e. no ghci exceptions
-- copied from: http://hackage.haskell.org/package/clash-prelude-0.10.7/docs/CLaSH-Prelude-DataFlow.html#v:fifoDF
-- =============================
fifoDF_mealy :: forall addrSize a .
     (KnownNat addrSize
     ,KnownNat (addrSize + 1)
     ,KnownNat (2 ^ addrSize))
  => (Vec (2^addrSize) a, BitVector (addrSize + 1), BitVector (addrSize + 1))
  -> (a, Bool, Bool)
  -> ((Vec (2^addrSize) a, BitVector (addrSize + 1), BitVector (addrSize + 1))
     ,(a, Bool, Bool))
fifoDF_mealy (mem,rptr,wptr) (wdata,winc,rinc) =
  ((mem',rptr',wptr'), (rdata,empty,full))
  where
    raddr = truncateB rptr :: BitVector addrSize
    waddr = truncateB wptr :: BitVector addrSize

    mem' | winc && not full = replace waddr wdata mem
         | otherwise        = mem

    rdata = mem !! raddr

    rptr' = rptr + boolToBV (rinc && not empty)
    wptr' = wptr + boolToBV (winc && not full)
    empty = rptr == wptr
    full  = msb rptr /= msb wptr && raddr == waddr

fifoDF' :: forall addrSize m n nm rate .
     (KnownNat addrSize,
     KnownNat n, KnownNat m,
     KnownNat (2 ^ addrSize),
     KnownNat (addrSize + 1),
     (m + n) ~ (2 ^ addrSize),
     KnownSymbol nm, KnownNat rate)
  => SNat (m + n) -- ^ Depth of the FIFO buffer. Must be a power of two.
  -> Vec m XillybusType      -- ^ Initial content. Can be smaller than the size of the
                  -- FIFO. Empty spaces are initialised with 'undefined'.
  -> DataFlow' ('Clk nm rate) Bool Bool XillybusType XillybusType
fifoDF' _ iS = DF $ \i iV oR ->
  let clk            = sclock
      initRdPtr      = 0
      initWrPtr      = fromIntegral (length iS)
      initMem        = iS ++ repeat 0 :: Vec (m + n) XillybusType
      initS          = (initMem,initRdPtr,initWrPtr)
      (o,empty,full) = mealyB' clk fifoDF_mealy initS (i,iV,oR)
  in  (o,not1 empty, not1 full)

-- =============================
-- helper functions
-- =============================


flipEndianness :: Int32 -> Int32
flipEndianness x = bitCoerce $ reverse $ (bitCoerce x :: Vec 4 (Unsigned 8)) :: XillybusType

b2b :: Bool -> Bit
b2b a    | a = 1
        | otherwise = 0

-- =====================================
-- Testing function (actual input fifo)     
-- =====================================
-- FIFO buffer empty/full test signals
dontCare = register 0 (dontCare) :: Signal XillybusType

false = register (False) (false) :: Signal Bool
falseN n = stimuliGenerator ((replicate n False) ++ (replicate d500 True))
falseNtrueN n1 n2 = stimuliGenerator ((replicate n1 False) ++ (replicate n2 True) ++ (replicate d500 False))
falseNtrueNfalseN n1 n2 n3 = stimuliGenerator ((replicate n1 False) ++ (replicate n2 True) ++ (replicate n3 False) ++ (replicate d500 True))

true = register (True) (true) :: Signal Bool
trueN n = stimuliGenerator ((replicate n True) ++ (replicate d500 False))
trueNfalseN n1 n2 = stimuliGenerator ((replicate n1 True) ++ (replicate n2 False) ++ (replicate d500 True))
trueNfalseNtrueN n1 n2 n3 = stimuliGenerator ((replicate n1 True) ++ (replicate n2 False) ++ (replicate n3 True) ++ (replicate d500 False))

-- FIFO buffer test case
inpSet1 = fifoDF' d512 ((10:: XillybusType):>2:>1:>0:>10:>2:>1:>1:>10:>2:>1:>2:>10:>2:>1:>3:>10:>0:>2:>1:>2:>10:>0:>2:>1:>6:>10:>1:>9:>1:>1:>1:>1:>1:>1:>1:>1:>1:>Nil)

inpSet2 = fifoDF' d512 ((10:: XillybusType):>0:>2:>20:>20:>10:>0:>2:>30:>30:>10:>0:>2:>40:>40:>10:>0:>2:>50:>50:>10:>0:>2:>6:>6:>10:>0:>2:>1:>6:>10:>0:>2:>1:>7:>10:>0:>2:>1:>8:>10:>0:>2:>1:>9:>10:>0:>2:>1:>10:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>10:>0:>2:>1:>1:>Nil)


-- test function 2
testArch' :: (DataFlow Bool Bool XillybusType XillybusType) -> Signal Bool -> Signal Bool -> Signal((XillybusType, Bool,XillybusType, Bool, Bool, Bool, Bool, Bool))
testArch' fifoBuffer outpFull inpEmpty = bundle ((x,yi,a,c,d,e,f,g))
        where
              outp' = topEntity $ bundle (fmap flipEndianness x, yi,outpFull) :: Signal (XillybusType, Bool, Bool, Bool, Bool, Bool, Bool)
              (a,b,c,d,e,f,g) = unbundle outp'
              yc = (.&&.) y c'
              c' = register False ((.&&.) c $ not1 inpEmpty)
              y' = register False y
              yi = (.||.) (not1 y) (inpEmpty)
              x' = register 0 x
              (x,y,_) =  (df fifoBuffer) dontCare false (yc)
              
testArch samples fifoBuffer outpFull inpEmpty = mapM_ putStrLn $ fmap show $ fmap (\((x,y,a,c,d,e,f,g)) -> ((x, b2b y,"||",flipEndianness a,b2b c,"||",b2b d,b2b e,b2b f,b2b g))) $ sampleN samples $ testArch' fifoBuffer outpFull inpEmpty