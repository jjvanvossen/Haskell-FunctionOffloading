module CLaSHTopEntityModule_manual where 

import CLaSH.Prelude  
import Offload.CLaSH.FunctionOffloadWrapper 
import Offload.Common.OffloadTypes
import OffloadableFunctions_manual 

{-# ANN topEntity 
  (defTop 
    { t_name     = "SoCKit_Offloaded_Haskell_functions" 
    , t_inputs   = ["data_in","empty_in","full_out"] 
    , t_outputs  = ["data_o","wren_out","rden_in","err_r_mux","err_r_s2p","err_w_p2s","err_w_demux"] 
    }) #-} 

topEntity = topWrapper ((functionWrapper ((pureDF mult),$(wordPackPureNats mult)))
        :>(functionWrapper ((pureDF matrixMult),$(wordPackPureNats matrixMult)))
        :>(functionWrapper ((mealyDF fir (repeat 0)),$(wordPackMealyNats fir)))
        :>Nil)