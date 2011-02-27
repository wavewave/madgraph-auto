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

makeRunName :: ProcessSetup -> RunSetup -> String 
makeRunName psetup rsetup = 
  let mprefix = case (model psetup) of 
        SM -> "SM"
        Wp -> "Wp"
        ZpH -> "Zp"
        Six -> "Six"
        Trip -> "Trip"
        AxiGluon -> "Axi" 
      masscoup =  case (param rsetup) of 
        SMParam -> "" 
        WpParam  m g -> "M"++show m++"G"++show g
        ZpHParam m g -> "M"++show m++"G"++show g 
        SixParam m g -> "M"++show m++"G"++show g
        TripParam m g -> "M"++show m++"G"++show g  
        AxiGluonParam m gvq gvt gaq gat -> "M"++show m++"Vq"++show gvq ++ "Vt"++show gvt ++ "Aq" ++ show gaq ++ "At" ++ show gat 
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

