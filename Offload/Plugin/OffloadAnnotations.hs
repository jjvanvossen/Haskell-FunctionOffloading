{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}

{-# OPTIONS_GHC -dynamic-too #-}

module Offload.Plugin.OffloadAnnotations 
(
 Offload(..),
)
where

import Data.Data (Data, Typeable)
import Data.Eq
import CLaSH.Prelude (BitPack, BitSize)
import GHC.TypeLits (KnownNat)

-- Annotations used to identify the offloadable functions in the Core plugin. The Mealy and Moore types are not fully implemented.
data Offload =  OffloadPure | OffloadMealy | OffloadMoore deriving (Data, Typeable, Eq)






