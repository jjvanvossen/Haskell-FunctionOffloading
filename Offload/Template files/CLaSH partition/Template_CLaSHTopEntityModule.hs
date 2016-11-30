-- ==============================================
-- Do not edit this initial part
-- ==============================================
module CLaSHTopEntityModule where 

import CLaSH.Prelude 
import Offload.Common.OffloadTypes 
import Offload.CLaSH.FunctionOffloadWrapper 

{-# ANN topEntity 
  (defTop 
    { t_name     = "SoCKit_Offloaded_Haskell_functions" 
    , t_inputs   = ["data_in","empty_in","full_out"] 
    , t_outputs  = ["data_o","wren_out","rden_in","err_r_mux","err_r_s2p","err_w_p2s","err_w_demux"] 
    }) #-} 
    
    
-- ==============================================
-- Edit below
-- ==============================================
-- add modules containing the offloadable functions. (Should be completely  CLaSH compilable)
import OffloadableFunctions 

-- fill in the offloadable functions vector here. See examples below.
topEntity = topWrapper  ((functionWrapper ((),$()))
                        :>Nil)




-- ==============================================
-- Example topEntity function
-- ==============================================
-- This example shows how to manually generate the CLaSH top-entity function. Directly below is an example with pure, mealy and moore functions.  
-- topEntity = topWrapper       ((functionWrapper ((pureDF fn_pure), $(wordPackPureNats fn_pure)))
--                              :>(functionWrapper ((mooreDF fn_moore initState), $(wordPackMealyNats fn_mealy)))
--                              :>(functionWrapper ((mooreDF fn_moore initState), $(wordPackMooreNats fn_moore)))
--                              :>Nil)

-- ==============================================
-- Tutorial
-- ==============================================
-- An offloadable function in the topWrapper vector argument is to be passed as an argument to the 'functionWrapper' function.

-- functionWrapper      :: 
--                      -> ((DataFlow Bool Bool arg res),(SNat argWords, SNat argRemBits, SNat resWords, SNat resRemBits))
--                      -> (XillybusType, Bool, Bool)
--                      -> (XillybusType, Bool, Bool, Bool, Bool)

-- ----------------------------------------------
-- What we need to insert here is the first tuple, i.e.:

--  ((DataFlow Bool Bool arg res),(SNat argWords, SNat argRemBits, SNat resWords, SNat resRemBits))

-- ----------------------------------------------
-- The first part of the tuple represents the offloadable function within a dataflow composition function from CLaSH:
-- http://hackage.haskell.org/package/clash-prelude-0.10.7/docs/CLaSH-Prelude-DataFlow.html

-- So 'pureDF' for a (i -> 0) typed offloadable function
-- 'mealyDF' for a (s -> i -> (s, o)) typed offloadable function. We also need to insert the initial state here.
-- 'mooreDF' for a ((s -> i -> s) -> (s -> o)) typed offloadable function. Here we also need to insert the initial state.

-- ----------------------------------------------
-- The second part of the tuple represents the type naturals related to the argument ('i') and result ('o') type of the offloadable function. It contains four SNats which represents the following:

-- SNat argWords: The amount of 32-bits words the type 'i' fits in. (i.e. (ceil ((finiteBitSize (a :: i)) quot 32)))
-- SNat argRemBits: The amount of bits used to extend the bit representation of type 'i' to a multiple of 32-bit words. (i.e. (32 - (rem (finiteBitSize (a :: i)) 32)))
-- Same goes for resWords and resRemBits, but they are meant for the 'o' type.

-- A Template Haskell function is available that calculates this for us. Do not forget to put the '$' operator in front of the operation to initiate Template Haskell at compile-time.
-- 'wordPackPureNats' for pure typed functions.
-- 'wordPackMealyNats' for mealy typed functions.
-- 'wordPackMooreNats' for moore typed functions.

-- To manually create these type naturals in the 4-tuple as following:
-- 1. type them as 'd1' (note: this is limited to d1024. For higher values see next method.)
-- 2. type them as '(snat :: SNat 1)'
-- i.e. ((Signed 13) -> (SFixed 8 8)) will result in (d1,d19,d1,d16)