{-# LANGUAGE ScopedTypeVariables #-}

module Work where

import System.IO
import System.Process
import System.Directory
import System.Posix.Unistd (sleep)

import Control.Monad

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Text.Printf

import Model 
import Machine

data ScriptSetup = SS { 
    scriptbase :: String
  , mg5base    :: String
  , workbase   :: String
}

data ProcessSetup = PS { 
    mversion :: ModelVersion
  , model   :: Model
  , process :: String
  , processBrief :: String  
  , workname :: String 
  }

data ClusterRunType = NoParallel | Parallel Int | Cluster String

data UserCutType = NoUserCutDefined | UserCutDefined

data RunSetup = RS { 
    param   :: Param
  , numevent :: Int
  , machine :: MachineType
  , rgrun   :: RGRunType
  , rgscale :: Double
  , match   :: MatchType
  , cut     :: CutType
  , pythia  :: PYTHIAType
  , usercut :: UserCutType
  , pgs     :: PGSType 
  , cluster :: ClusterRunType 
  , setnum  :: Int 
}


createWorkDir :: ScriptSetup -> ProcessSetup -> IO ()
createWorkDir ssetup psetup = do 
  putStrLn $ "set up a working directory" 
  let tempdir = scriptbase ssetup ++ "working/"
      processfilecontent = makeProcessFile (model psetup) (mversion psetup) (process psetup) (workname psetup)
  writeFile (tempdir ++ "proc_card_mg5.dat") processfilecontent
  
  setCurrentDirectory (mg5base ssetup)
  readProcess ("bin/mg5") [tempdir ++ "proc_card_mg5.dat"] ""
  putStrLn "Wait Two Seconds"  
  sleep 2
  putStrLn $ "moving directory" 
 	++ (mg5base ssetup ++ workname psetup) 
 	++ " to " 
	++ (workbase ssetup ++ workname psetup) 
  renameDirectory (mg5base ssetup ++ workname psetup) (workbase ssetup ++ workname psetup) 
  return ()


makeRunName :: ProcessSetup -> RunSetup -> String 
makeRunName psetup rsetup = 
  let mprefix = case (model psetup) of 
        Wp -> "Wp"
        ZpH -> "Zp"
        Six -> "Six"
        Trip -> "Trip"
      masscoup =  case (param rsetup) of 
        WpParam  m g -> "M"++show m++"G"++show g
        ZpHParam m g -> "M"++show m++"G"++show g 
        SixParam m g -> "M"++show m++"G"++show g
        TripParam m g -> "M"++show m++"G"++show g  
      machineName = case (machine rsetup) of 
        TeVatron -> "TeVa" 
        LHC7 -> "LHC7"
        LHC14 -> "LHC14"
      matchName = case (match rsetup) of 
        MLM -> "MLM"
        NoMatch -> "NoMatch"
      cutName = case (cut rsetup) of 
        NoCut -> "NoCut"
        DefCut -> "DefCut"
        KCut -> "KCut"
  in  mprefix++masscoup++"_"++processBrief psetup++"_"++machineName++"_"++matchName++"_"++cutName++"_Set" ++ show (setnum rsetup)  
        
existThenRemove :: FilePath -> IO () 
existThenRemove fp = do 
  b <- doesFileExist fp 
  if b 
    then removeFile fp
    else return () 

cardPrepare :: ScriptSetup -> ProcessSetup -> RunSetup -> IO () 
cardPrepare ssetup psetup rsetup = do 
  let taskname = makeRunName psetup rsetup 
  
  putStrLn $ "prepare for cards for " ++ taskname
  
  
  let carddir = workbase ssetup ++ workname psetup ++ "/Cards/"
      tpath   = scriptbase ssetup ++ "template/"
  
  -- erase previous run 
  existThenRemove (carddir ++ "param_card.dat") 
  existThenRemove (carddir ++ "run_card.dat") 
  existThenRemove (carddir ++ "pythia_card.dat") 
  existThenRemove (carddir ++ "pgs_card.dat")
  
  paramcard  <- paramCardSetup 
                  tpath
                  (model psetup)
                  (param rsetup)
  
  runcard    <- runCardSetup 
                  tpath
                  (machine rsetup) 
                  (cut     rsetup) 
                  (match   rsetup) 
                  (rgrun   rsetup) 
                  (rgscale rsetup) 
		  (numevent rsetup) 
                  
  pythiacard <- pythiaCardSetup 
                  tpath
                  (match   rsetup)
                  (pythia  rsetup) 
                  
  pgscard    <- pgsCardSetup
                  tpath
                  (machine rsetup)
                  (pgs     rsetup) 
                  
  writeFile (carddir ++ "param_card.dat") paramcard
  writeFile (carddir ++ "run_card.dat")   runcard

  case pythiacard of 
    Nothing -> return () 
    Just str -> writeFile (carddir ++ "pythia_card.dat") str
    
  case pgscard  of 
    Nothing -> return () 
    Just str -> case usercut rsetup of 
      NoUserCutDefined -> writeFile (carddir ++ "pgs_card.dat") str
      UserCutDefined   -> writeFile (carddir ++ "pgs_card.dat.user") str


generateEvents :: ScriptSetup -> ProcessSetup -> RunSetup -> IO () 
generateEvents ssetup psetup rsetup = do 

  let taskname = makeRunName psetup rsetup 
  
  putStrLn $ "generating event for " ++ taskname
  
  
  
  setCurrentDirectory (workbase ssetup ++ workname psetup)
  case cluster rsetup of
    NoParallel -> readProcess ("bin/generate_events") ["0", taskname] ""
    Parallel ncore -> readProcess ("bin/generate_events") ["2", show ncore, taskname] ""
    Cluster cname -> readProcess ("bin/generate_events") ["1", cname, taskname] "" 
  return ()


  
