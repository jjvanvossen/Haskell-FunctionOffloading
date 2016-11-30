{-# LANGUAGE ScopedTypeVariables #-}

module Offload.CLaSH.FunctionOffloadWrapper
(
        functionWrapper,
        topWrapper,
)
where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import Offload.Common.OffloadTypes


-- 

-- =====================================
-- Misc. functions
-- ===================================== 

-- Reverse the byte order of a 32-bits integer. 
flipEndianness :: Int32 -> Int32
flipEndianness x = bitCoerce $ reverse $ (bitCoerce x :: Vec 4 (Unsigned 8))

-- | 'unzip5' transforms a vector of fivelets into a vector of first components,
-- a vector of second components, and a vector of third components.
unzip5 :: Vec n (a,b,c,d,e) -> (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e)
unzip5 xs = ( map (\(v,_,_,_,_) -> v) xs
            , map (\(_,w,_,_,_) -> w) xs
            , map (\(_,_,x,_,_) -> x) xs
            , map (\(_,_,_,y,_) -> y) xs
            , map (\(_,_,_,_,z) -> z) xs
            )
            
-- ==========================================================================               
-- ==========================================================================
-- function wrapper & seralization
-- ==========================================================================
-- ==========================================================================             
-- =====================================
-- Function that de-serializes the incoming data
-- ===================================== 
ser2par :: forall arg a1 a2 a3 a4 a5.(WordPackConstraints arg a1 a2 a3 a4, KnownNat a5, a5 ~ (a2 + 1))
                => (SNat a2, SNat a4)
                -> (Vec a5 (XillybusType), Int32, Bool, Bool, Bool, Bool) 
                -> (XillybusType, Bool, Bool)
                -> ((Vec a5 (XillybusType), Int32, Bool, Bool, Bool, Bool), (Bool, arg, Bool, Bool))
ser2par (argWords, argRem) (input, readOffset, isReading, isValid, allRead, errorFlag) (r_data, r_valid, fn_ready) = ((input', readOffset', isReading',isValid', allRead', errorFlag'),(mux_ready,fn_data, fn_valid, errorFlag'))
    where
        -- outputs signals
        -- boolean indicating that this function is ready to receive from multiplexer
        mux_ready                                               = isReading'
        
        -- boolean indicatinng the data is available for the offloaded function
        fn_valid                                                = isValid
        
        -- remove data size element and (un)pack to arg type
        fn_data                                                 = unpack $ resize $ pack $ drop d1 input  :: arg
                                                                                               
        -- states            
        -- vector used to store the serialized data
        input'          | (.&.) isReading r_valid               = replace readOffset r_data input
                        | otherwise                             = input
        
        -- integer indicating the current offset for writing in the input vector
        readOffset'     | (.&.) r_valid $ (.&.) isReading $ not ets     = readOffset + 1
                        | allRead'                                       = 0 -- reset
                        | otherwise                                     = readOffset
        
        -- when data is not all read (or valid), then reading mode is active                        
        isReading'                                              = not $ (.|.) allRead' isValid
        
        -- asserts when all data has been read until isValid is asserted               
        allRead'        | isValid                               = False
                        | (.|.) allRead $ (.&.) ets $ r_valid   = True
                        | otherwise                             = False
        
        -- only valid for one clock cycle if dataflow function is ready and all data has been read.        
        isValid'                                                = (.&.) fn_ready allRead' 
        
        -- error signal indicating an error in the received data size value
        errorFlag'      | errorFlag                                     = True -- keep displaying error
                        | (.&.) (curSize /= 0) (curSize /= (argSize))   = True -- if data size doesn't match
                        | otherwise                                     = False  
                        where
                                curSize = input!!(0 :: XillybusType) -- get current size value                               
                                
        -- Misc.
        -- check if end of reading data is reached
        ets                                                     = readOffset == argSize 
        argSize                                                 = fromIntegral $ maxIndex input :: Int32 

-- =====================================
-- Function that serializes the result of the associated offloaded function
-- ===================================== 
par2ser :: forall res a1 a2 a3 a4 a5.(WordPackConstraints res a1 a2 a3 a4, KnownNat a5, a5 ~ (a2 + 1), KnownNat (a4 + a1), (a2 * 32) ~ (a4 + a1))
                => (SNat a2, SNat a4)
                -> (Vec a5 (XillybusType), Int32, Bool, Bool) 
                -> (Bool, res, Bool)
                -> ((Vec a5 (XillybusType), Int32, Bool, Bool), (XillybusType, Bool, Bool, Bool))
par2ser (resWords, resRem) (output, writeOffset, isWriting, errorFlag) (w_ready, fn_data, fn_valid) = ((output', writeOffset', isWriting', errorFlag'),(w_data, isWriting ,fn_ready, errorFlag'))
    where
        -- output
        -- boolean signal that indicates the data is valid and should be written to the FIFO buffer
        w_wren                                                  = (.&.) isWriting $ w_ready
        
        -- current output data to the multiplexer
        w_data          | w_wren                                = (output!!writeOffset) :: XillybusType
                        | otherwise                             = 0 :: XillybusType 
        
        -- boolean signal that indicates that the offloaded function may be executed as the data is ready to be received
        fn_ready                                                = not $ (.&.) isWriting' isWriting
        
        -- states
        -- serialized representation of the received offloadable function result + the serialized data length in the first index (as required in the multiplexer).
        output'         | (.&.) fn_valid $ not isWriting        = (resSize :> (unpack $ resize $ pack fn_data)) :: Vec a5 (XillybusType)
                        | otherwise                             = output
        
        -- current writing offset of the serialized result
        writeOffset'    | (.&.) w_wren $ writeOffset < resSize  = writeOffset + 1
                        | isWriting'                            = writeOffset
                        | otherwise                             = 0
        -- boolean indicating that data is available that should be written out.
        isWriting'      | (.&.) fn_valid $ writeOffset == 0       = True
                        | (.|.) (not w_ready) $ writeOffset < resSize  = isWriting
                        | otherwise                             = False
        
        -- Error flag, but no errors should/can occur here in this proof of concept..        
        errorFlag'                                              = False
        
        -- other                
        resSize                                                 = fromIntegral $ maxIndex output :: Int32

-- =====================================
-- Function that wraps the serialisation blocks around a offloadable function
-- ===================================== 
functionWrapper :: forall arg res a1 a2 a3 a4 a5 b1 b2 b3 b4 b5.(WordPackConstraints arg a1 a2 a3 a4, WordPackConstraints res b1 b2 b3 b4, KnownNat a5, a5 ~ (a2 + 1), KnownNat b5, b5 ~ (b2 + 1),KnownNat (b4 + b1), (b2 * 32) ~ (b4 + b1))
                => ((DataFlow Bool Bool arg res),(SNat a2, SNat a4, SNat b2, SNat b4))
                -> Signal (XillybusType, Bool, Bool)
                -> Signal (XillybusType, Bool, Bool, Bool, Bool)
functionWrapper (f,(argWords, argRem, resWords, resRem)) inp    = outp 
        where 
                -- Input/Output signals
                (r_data, r_empty, w_full)                       = unbundle inp
                outp                                            = bundle (w_data, w_wren, r_rden, errS2P, errP2S)
                
                -- Read/ser2par wrapper
                (r_rden, fn_data_in, fn_valid_in, errS2P)       = mealyB (ser2par (argWords, argRem)) ((repeat 0 :: Vec a5 XillybusType), 0, False, False, False, False) (r_data, r_empty, fn_ready_in)
                
                -- Write/par2ser wrapper 
                (w_data, w_wren, fn_ready_out, errP2S)          = mealyB (par2ser (resWords, resRem)) ((repeat 0 :: Vec b5 XillybusType), 0, False, False) (w_full, fn_data_out, fn_valid_out)
                
                -- wrapped function
                (fn_data_out, fn_valid_out, fn_ready_in)        = df f fn_data_in fn_valid_in fn_ready_out

       
-- ==========================================================================               
-- ==========================================================================
-- Top wrapper & muxers
-- ==========================================================================
-- ==========================================================================   
-- =====================================
-- 
-- ===================================== 
readDemux       :: forall n .(KnownNat n)
                => (Int32, Bool, Int32, Bool, Bool, Int32, Bool)
                -> (XillybusType, Bool, Vec n Bool)
                -> ((Int32, Bool, Int32, Bool, Bool, Int32, Bool),(Vec n XillybusType, Vec n Bool, Bool, Bool))
readDemux (rdIndex, isReading, fnID, rdEn, errorFlag, readData, dataValid) (fifo_r_data, fifo_r_empty, fn_r_rdenVec) = ((rdIndex', isReading', fnID', rdEn', errorFlag',readData', dataValid'),(fn_r_dataVec, fn_r_validVec, fifo_r_rden, errorFlag))
        where
                -- output signals

                fifo_r_rden                                     = rdEn'
                
                fn_r_dataVec                                    = repeat readData
                
                fn_r_validVec   | (.|.) (rdIndex == -1) (rdIndex > 0)   = replace fnID dataValid $ repeat False 
                                | otherwise                             = repeat False

                -- states          
                readData'       | rdEn                          = fifo_r_data'
                                | otherwise                     = readData 
                
                dataValid'                                      = rdEn
                
                rdIndex'        | not isReading                 = -4 --init/reset
                                | not rdEn                      = rdIndex -- retain index if nothing is read
                                | rdIndex == -1                 = readData -- set expected length 
                                | rdIndex > 0                   = rdIndex - 1 --initial reading increment
                                | rdIndex < 0                   = rdIndex + 1 --data reading decrement
                                | otherwise                     = rdIndex
                
                isReading'      | rdIndex == (-4) = True
                                | rdIndex' == 1 = False
                                | otherwise = isReading
                
                fnID'           | rdIndex == -2                 = readData
                                | rdIndex == -4                 = -1
                                | otherwise                     = fnID
                
                rdEn'           | (.&.) cond1 (rdIndex == -2)   = fn_r_rdenVec!!fnID' -- wait until 'ser2par' is ready
                                | (.&.) cond1 $ (.&.) (rdIndex == -3) (rdIndex' == -2)        = False -- wait for fnID is received
                                | cond1                         = True -- start and continued reading of new message
                                | otherwise                     = False
                                where
                                        cond1                   = (.&.) isReading' (not fifo_r_empty)     

                errorFlag'      | errorFlag                                                             = True
                                | not rdEn                                                              = False
                                | (.&.) (rdIndex == -3) $ readData /= (fromIntegral beginFlag)          = True
                                | (.&.) (rdIndex == -2) $ readData < 0                                  = True
                                | (.&.) (rdIndex == -2) $ readData > vecSize                            = True
                                | (.&.) (rdIndex == -1) $ readData < 1                                  = True
                                | otherwise                                                             = False                
                                              
                -- misc
                vecSize         = fromIntegral $ snatToInteger (snat :: SNat n) -- amount of offloaded functions
                fifo_r_data'    = flipEndianness fifo_r_data


                
                
-- =====================================
-- 
-- =====================================   
writeMux      :: forall n .(KnownNat n)
                => (Int32, Bool, Int32, Bool, Bool)
                -> (Vec n XillybusType, Vec n Bool, Bool)
                -> ((Int32, Bool, Int32, Bool, Bool),(XillybusType, Bool, Vec n Bool, Bool))
writeMux (wrIndex, isWriting, fnID, wrEn,errorFlag) (fn_w_dataVec, fn_w_wrenVec, fifo_w_full) = ((wrIndex', isWriting', fnID', wrEn',errorFlag'),(fifo_w_data, fifo_w_wren, fn_w_readyVec, errorFlag))
        where
                -- output signals
                -- the data output to the FIFO buffer
                fifo_w_data'    | wrIndex == (-3)                       = fromIntegral beginFlag
                                | wrIndex == (-2)                       = fnID
                                | otherwise                             = fn_w_dataVec!!fnID
                                
                -- Boolean that indicates that the data should be written in the FIFO buffer 
                fifo_w_wren     = wrEn'
                
                -- vector of all par2ser ready signals
                fn_w_readyVec   | (.&.) isWriting $ (.&.) (wrIndex > -2) $ not fifo_w_full = replace fnID True $ repeat False :: Vec n Bool
                                | otherwise = repeat False :: Vec n Bool
                                
                -- states      
                -- integer that indicates the current index of the serialized data that is being put into the FIFO buffer
                wrIndex'        | not isWriting = -3 -- reset
                                | fifo_w_full = wrIndex -- wait if fifo is full
                                | wrIndex == -1 = fn_w_dataVec!!fnID -- first data is the length of the serialized message
                                | (.&.) wrEn' $ wrIndex > 0 = wrIndex - 1
                                | (.&.) wrEn' $ wrIndex < 0 = wrIndex + 1
                                | otherwise = wrIndex
                
                -- boolean that indicates if currently serializing data from an offloaded function is being read and written to the FIFO buffer
                isWriting'      | (.&.) (fn_w_wrenVec!!fnID) $ wrIndex == (-3) = True
                                | (.&.) (not $ fn_w_wrenVec!!fnID) $ wrIndex == 0 = False
                                | otherwise = isWriting
                
                -- Indicator of current function ID of which the result is being serialized. 
                -- It is implemented in a round-robin scheduling fashion so that all functions get their turn to output the result, but this might not be efficient for a large amount of offloaded functions.
                fnID'           | (.&.) (fnID >= 0) $ (.&.) (fnID < (vecSize-1)) cond = fnID + 1
                                | cond = 0
                                | otherwise = fnID
                                where
                                        cond = (.&.) (not isWriting') $ wrIndex == (-3)
                
                -- indicates if current data should be inserted into the output FIFO buffer
                wrEn'           | (.&.) isWriting $ (.&.) (fn_w_wrenVec!!fnID) $ not fifo_w_full = True
                                | otherwise = False
                
                -- Error flags, but no errors should/can occur here in this proof of concept..
                errorFlag'      | errorFlag = True
                                | otherwise = False
                                

                -- misc.
                vecSize         = fromIntegral $ snatToInteger (snat :: SNat n) -- amount of offloaded functions
                fifo_w_data     = flipEndianness fifo_w_data'
                

-- =====================================
-- 
-- =====================================                 
topWrapper      :: forall n .(KnownNat n)
                => (Vec n (Signal(XillybusType, Bool, Bool) -> Signal(XillybusType, Bool, Bool, Bool, Bool)))
                -> Signal (XillybusType, Bool, Bool)
                -> Signal (XillybusType, Bool, Bool, Bool, Bool, Bool, Bool)     
topWrapper fs input = output
        where
                (fifo_r_data, fifo_r_empty, fifo_w_full)        = unbundle input -- unbundle inputs
                output                                          = bundle (fifo_w_data, fifo_w_wren, fifo_r_rden,err0, err1, err2, err3) -- bundle output
                err1                                            = foldl (.||.) (signal False) fn_r_errVec -- combine all error signals of the 'ser2par' functions
                err2                                            = foldl (.||.) (signal False) fn_w_errVec -- combine all error signals of the 'par2ser' functions
                
                -- read demultiplexer
                readDemuxInit                                   = (-4,False,-1,False,False,0, False) -- initial state
                (fn_r_dataVec, fn_r_validVec, fifo_r_rden,err0) = mealyB readDemux readDemuxInit (fifo_r_data, fifo_r_empty, fn_r_rdenVec)     
                
                -- write multiplexer
                writeMuxInit                                    = (-3,False,0,False,False) -- initial state
                (fifo_w_data, fifo_w_wren, fn_w_readyVec,err3)  = mealyB writeMux writeMuxInit (fn_w_dataVec, fn_w_wrenVec, fifo_w_full)
                
                -- Vector of funtionWrapper and their offloaded 
                fnInputVec                                      = map bundle $ zip3 (unbundle fn_r_dataVec) (unbundle fn_r_validVec) (unbundle fn_w_readyVec) -- zip and bundle input signals 
                (fn_w_dataVec, fn_w_wrenVec, fn_r_rdenVec)      = (bundle outp0, bundle outp1, bundle outp2)
                (outp0,outp1,outp2,fn_r_errVec,fn_w_errVec)     = unzip5 $ map unbundle $ zipWith id fs fnInputVec -- unbundle and unzip resulting signals

                