{-# LANGUAGE DataKinds #-}

module Offload.Plugin.OffloadPlugin where

import GHC hiding (exprType)
import GhcPlugins hiding (substTy)
import Data.Data  (Data)
import GHC.TypeLits (KnownNat, Nat)
import Class (Class, classTyCon, classATs, className)
import PrelNames (knownNatClassName)
import MonadUtils (mapAccumLM)
import Linker (linkModule)
import GHC.Paths (libdir)
import MonadUtils
import Outputable
import HscMain
import HscTypes
import System.Directory
import Data.List (find)
import Offload.Plugin.OffloadAnnotations (Offload(..))

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install str todo = do
  reinitializeGlobals    
  return (CoreDoPluginPass "Function Offloading" pass : todo)
  

pass :: ModGuts -> CoreM ModGuts
pass g = do
          dflags <- getDynFlags
          putMsgS "===================================================="
          putMsgS "Automated offloading Core plugin"
          putMsgS "Version: 0.1 (Proof of concept)"
          putMsgS "Created by: J.J. van Vossen, 2016"
          putMsgS "Created for Emsys MSc. thesis @ University of Twente"
          putMsgS "===================================================="
          
          -- extract in-scope BitPack & KnownNat classes.
          inscopeClasses <- extractClasses dflags $ mg_binds g

          -- create CLaSH top entity module (part 1)
          _ <- liftIO $ initTopEntityMod $ showSDoc dflags $ ppr $ moduleName $ mg_module g
          
          -- pass over each binding and transform annotated functions.
          (_,offloadPass) <- mapAccumLM (replaceFunction dflags g inscopeClasses) 0 $ mg_binds g
          
          -- finish CLaSH top entity module (part 3)
          _ <- liftIO $ finishTopEntityMod
          
          -- return modulebindings
          return (g {mg_binds = offloadPass})
          
-- Function to extract in-scope BitPack & KnownNat classes.          
extractClasses :: DynFlags -> [CoreBind] -> CoreM (Class, Class)
extractClasses dflags binds = do
                let corebind@(NonRec b fn) = case (find (\x@(NonRec b' fn') -> (showSDoc dflags $ ppr b') == "makeClassesInScope") binds) of
                        Just result -> result
                        Nothing     -> errorMessages 1
                        
                let allClassesInDict = fst $ splitFunTysN 2 $ exprType fn
                
                let bitPackClass = case (getClassPredTys_maybe $ allClassesInDict!!0) of
                        Just result -> fst $ result
                        Nothing     -> errorMessages 0
                        
                let knownNatClass = case (getClassPredTys_maybe $ allClassesInDict!!1) of
                        Just result -> fst $ result
                        Nothing     -> errorMessages 0
                        
                let inscopeClasses      | pprClasses == "(BitPack, KnownNat)" = extractClasses
                                        | otherwise = errorMessages 0
                                        where
                                                pprClasses = showSDoc dflags $ ppr extractClasses
                                                extractClasses = (bitPackClass, knownNatClass)
                        
                return (inscopeClasses)

-- Error reporting function                
errorMessages index     | index == 0 = error "The required 'makeClassesInScope' function has been modified. Please advice the manual to revert to the original function. (Required to get the BitPack and KnownNat classes in-scope)."
                        | index == 1 = error "The 'makeClassesInScope' function was not found in current module. Please read the manual and insert this function correctly. (Required to get the BitPack and KnownNat classes in-scope)."
                        | otherwise = error "This should not happen!"              
                
-- Main function of the core plugin that is mapped over the list of corebinds          
replaceFunction :: DynFlags -> ModGuts -> (Class, Class) -> Int -> CoreBind -> CoreM (Int,CoreBind)
replaceFunction dflags guts (bitPackClass, knownNatClass) offloadIndex bndr@(NonRec b fn)  = do
        anns <- annotationsOn guts b :: CoreM [Offload]
        if (null anns) then do
                return (offloadIndex, bndr) -- no annotation found
        else do
                if ((head anns) == OffloadMealy) then do
                        putMsgS "===================================================="
                        putMsgS ("Mealy offload target found: " ++  showSDoc dflags (ppr b))
                        putMsgS "----------------------------------------------------"
                        putMsgS "Unfortunately this is not included in this automatic offloading proof of concept due to the fact that the Haskell program itself should be modified to skip the process of updating the mealy machine states. Try manual offloading instead."
                        putMsgS "===================================================="
                        return ((offloadIndex+1), bndr)
                else if ((head anns) == OffloadMoore) then do
                        putMsgS "===================================================="
                        putMsgS ("Moore offload target found: " ++  showSDoc dflags (ppr b))
                        putMsgS "----------------------------------------------------"
                        putMsgS "Unfortunately this is not included in this automatic offloading proof of concept due to the fact that the original Haskell program itself should be modified to remove the process of updating the moore machine states. Try manual offloading instead."
                        putMsgS "===================================================="
                        return ((offloadIndex+1), bndr)        
                else if ((head anns) == OffloadPure) then do                                              
                        putMsgS "===================================================="
                        putMsgS ("Pure offload target found: " ++  showSDoc dflags (ppr b))
                        putMsgS "----------------------------------------------------"
                        putMsgS "It will be offloaded exactly as given."
                        
                        -- ===============
                        -- fnId generation
                        let fnId = mkIntExprInt  dflags (offloadIndex)
                        putMsgS $ "Its function ID is number: " ++ (show offloadIndex)
                        
                        -- ===============
                        -- get types
                        let (_,fnTypeOrig) = splitForAllTys $ exprType fn -- remove forall a. ()'s
                        let (fnTypeOrig',fnResT) = splitFunTys fnTypeOrig
                        let fnArgT = last fnTypeOrig'
                        putMsgS ("The function argument type is: " ++  showSDoc dflags (ppr fnArgT))
                        putMsgS ("The function result type is: " ++  showSDoc dflags (ppr fnResT))
                        putMsgS "----------------------------------------------------"
                        
                        putMsgS "Generating replacement function..."
                        
                        --create temporary directory for generating the new function
                        _ <- liftIO $ createDirectoryIfMissing False "Offload/Plugin/Temp/"
                        
                        -- create offload function replacement in a new Haskell module
                        _ <- liftIO (mkTempOffloadMods dflags offloadIndex fnArgT fnResT)
                        
                        -- compile created offload function to coremodule
                        offloadMod <- liftIO $ compileOffloadMods

                        -- get the offload function core bind
                        let newBndr@(NonRec bo' fno') = last $ cm_binds offloadMod

                        -- Recreate the subparts of the replacement offload function type with the in-scope Classes.
                        let bitSizeA = mkBitSizeTy bitPackClass fnArgT
                        let bitSizeB = mkBitSizeTy bitPackClass fnResT
                        --let kNatBitSizeA = coerceTyBitPack bitPackClass fnArgT
                        --let kNatBitSizeB = coerceTyBitPack bitPackClass fnResT
                        
                        let bitPackPredA = mkClassPred bitPackClass [fnArgT]     
                        let bitPackPredB = mkClassPred bitPackClass [fnResT]
                        
                        let kNatBSizePredA = mkClassPred knownNatClass [bitSizeA]
                        let kNatBSizePredB = mkClassPred knownNatClass [bitSizeB]
                        
                        -- create new type
                        let newFnType = mkFunTys [bitPackPredA,bitPackPredB,kNatBSizePredA,kNatBSizePredB, fnArgT] fnResT
                        
                        putMsgS "Replacing original function..."
                        -- ===============
                        -- update original corebind
                        let bndrNew@(NonRec bRes fnRes) =  (NonRec (setVarType b newFnType) fno')
                        putMsgS "----------------------------------------------------"
                        
                        -- add function to CLaSH top entity module (part 2)
                        putMsgS "Inserting original function in CLaSH top-entity file..."
                        _ <- liftIO $ appendTopEntityMod $ showSDoc dflags $ ppr $ b
                        
                        -- remove the temporary directory
                        _ <- liftIO $ removeDirectoryRecursive   "Offload/Plugin/Temp/" -- can fail if text file is busy!!
                        
                        putMsgS ("Offloading pass completed for pure function: " ++  showSDoc dflags (ppr b))
                        putMsgS "===================================================="
                        
                        -- return modified corebind
                        return ((offloadIndex+1), bndrNew)
                else do
                        error "This should not happen"               
replaceFunction _ _ _ index bndr  = return (index,bndr)  -- catch empty/incorrect corebinds                                                  


-- Check if function bndr is annotated with the correct annotation
annotationsOn :: Data a => ModGuts -> CoreBndr  -> CoreM [a]
annotationsOn guts bndr = do
        anns <- getAnnotations deserializeWithData guts
        return $ lookupWithDefaultUFM anns [] (varUnique bndr)
          
-- ============================================================
-- Generate the KnownNat constaint
-- ============================================================

mkBitSizeTy :: Class -> Type -> Type
mkBitSizeTy clas ty = mkTyConApp (head $ classATs clas) [ty]       

coerceTyBitPack :: Class -> Type -> Type
coerceTyBitPack clas ty = mkTyConApp (classTyCon clas) [ty]

                
-- ============================================================
-- Get the coremodule of a given file. 
-- ============================================================        
compileOffloadMods = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        runGhcT (Just libdir) $ do
                dflags <- getProgramDynFlags 
                setSessionDynFlags dflags -- get package state
                offloadMod <- compileToCoreModule  "Offload/Plugin/Temp/TempModuleStage1.hs"
                return offloadMod
              

-- ============================================================
-- Create temporary offload function module
-- ============================================================         
mkTempOffloadMods :: DynFlags -> Int -> Type -> Type -> IO ()
mkTempOffloadMods dflags fnId inpType outpType = do
        let offloadTypeModStr = createOffloadTypeMod (showSDoc dflags (ppr inpType)) (showSDoc dflags (ppr outpType))
        writeFile "Offload/Plugin/Temp/TempModuleStage0.hs" offloadTypeModStr 
        
        let offloadExprModStr = createOffloadExprMod fnId (showSDoc dflags (ppr inpType)) (showSDoc dflags (ppr outpType))
        writeFile "Offload/Plugin/Temp/TempModuleStage1.hs" offloadExprModStr 
        
-- Create a temporary stage 0 module which is used by Template Haskell in the stage 1 module.
createOffloadTypeMod :: String -> String -> String
createOffloadTypeMod inpType outpType = "{-# LANGUAGE DataKinds, ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}\n {-# OPTIONS_GHC -dynamic-too #-} \nmodule Offload.Plugin.Temp.TempModuleStage0 where \nimport CLaSH.Prelude \nimport Offload.Haskell.OffloadFunctions \n\noffloadTypeTemplate :: (" ++ inpType ++ ") -> (" ++ outpType ++ ") \noffloadTypeTemplate inp = undefined"

-- Create the stage 1 module containing the actual replacement function for the annotated offloadable fucntion
createOffloadExprMod :: Int -> String -> String -> String
createOffloadExprMod fnId inpType outpType = "{-# LANGUAGE DataKinds, ScopedTypeVariables, FlexibleContexts, TypeFamilies, TemplateHaskell #-}\n {-# OPTIONS_GHC -dynamic-too #-} \nmodule Offload.Plugin.Temp.TempModuleStage1 where \nimport Language.Haskell.TH \nimport CLaSH.Prelude \nimport Offload.Haskell.OffloadFunctions \nimport Offload.Common.OffloadTypes \nimport Offload.Plugin.Temp.TempModuleStage0 \n\noffloadFunction :: (BitPack (" ++ inpType ++ "), BitPack (" ++ outpType ++ "), KnownNat (BitSize (" ++ inpType ++ ")), KnownNat (BitSize (" ++ outpType ++ "))) => (" ++ inpType ++ ") -> (" ++ outpType ++ ") \noffloadFunction inp = offloadTemplate (" ++ (show fnId) ++ " :: Int) $(wordPackPureNats offloadTypeTemplate) inp"


-- ============================================================
-- Create CLaSH top-entity module used for compiling the hardware partition of the co-design
-- ============================================================ 
-- Initialize top-entity module for the CLaSH compiler.
initTopEntityMod :: String -> IO ()
initTopEntityMod moduleName = do
        writeFile "CLaSHTopEntityModule_AutoGenerated.hs" $ "module CLaSHTopEntityModule_AutoGenerated where \n\nimport CLaSH.Prelude \nimport Offload.Common.OffloadTypes \nimport Offload.CLaSH.FunctionOffloadWrapper \nimport " ++ moduleName ++ " \n\n{-# ANN topEntity \n  (defTop \n    { t_name     = \"SoCKit_Offloaded_Haskell_functions\" \n    , t_inputs   = [\"data_in\",\"empty_in\",\"full_out\"] \n    , t_outputs  = [\"data_o\",\"wren_out\",\"rden_in\",\"err_r_mux\",\"err_r_s2p\",\"err_w_p2s\",\"err_w_demux\"] \n    }) #-} \n\ntopEntity = topWrapper ("

-- Function to append a correctly annotated offloadable function to the top-entity module for ClaSH compilation.
appendTopEntityMod :: String -> IO ()
appendTopEntityMod functionName = do
        appendFile "CLaSHTopEntityModule_AutoGenerated.hs" $ "(functionWrapper ((pureDF " ++ functionName ++ "),$(wordPackPureNats " ++ functionName ++ "))):>\n " 

-- Function that finalizes the top-entity module for ClaSH compilation.        
finishTopEntityMod :: IO ()
finishTopEntityMod = do
        appendFile "CLaSHTopEntityModule_AutoGenerated.hs" "Nil)"

