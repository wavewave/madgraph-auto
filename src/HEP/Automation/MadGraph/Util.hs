module HEP.Automation.MadGraph.Util where

import System.Directory

import HEP.Automation.MadGraph.Model 
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.SetupType

existThenRemove :: FilePath -> IO () 
existThenRemove fp = do 
  b <- doesFileExist fp 
  if b 
    then removeFile fp
    else return () 

makeRunName :: (Model a) => ProcessSetup a -> RunSetup a -> String 
makeRunName psetup rsetup = 
  let mprefix = briefShow (model psetup)  
      masscoup = briefParamShow (param rsetup) 
      machineName = case (machine rsetup) of 
        TeVatron -> "TeVa" 
        LHC7 -> "LHC7"
        LHC14 -> "LHC14"
        TeVatronParton -> "TeVaPtn"
      matchName = case (match rsetup) of 
        MLM -> "MLM"
        NoMatch -> "NoMatch"
      cutName = case (cut rsetup) of 
        NoCut -> "NoCut"
        DefCut -> "DefCut"
        KCut -> "KCut"
  in  mprefix++masscoup++"_"++processBrief psetup++"_"++machineName++"_"++matchName++"_"++cutName++"_Set" ++ show (setnum rsetup)  

