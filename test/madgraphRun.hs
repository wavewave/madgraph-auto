module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader 
import Control.Monad.Error
import System.FilePath 
import System.Directory 
import System.Log.Logger
-- 
-- import HEP.Automation.MadGraph.Card
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Model.SM
-- import HEP.Automation.MadGraph.Model.ADMXQLD211
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
import HEP.Parser.LHE.Sanitizer.Type
import HEP.Storage.WebDAV
-- 
import qualified Paths_madgraph_auto as PMadGraph
import qualified Paths_madgraph_auto_model as PModel

-- |  
getScriptSetup :: IO ScriptSetup
getScriptSetup = do 
  homedir <- getHomeDirectory
  mdldir <- (</> "template") <$> PModel.getDataDir
  rundir <- (</> "template") <$> PMadGraph.getDataDir 
  return $ 
    SS { modeltmpldir = mdldir 
       , runtmpldir = rundir 
       , sandboxdir = homedir </> "temp/montecarlo/sandbox"
       , mg5base    = homedir </> "temp/montecarlo/MG5_aMC_v2_1_0"
       , mcrundir   = homedir </> "temp/montecarlo/mcrun"
       , pythia8dir = ""
       , pythia8toHEPEVT = "" 
       , hepevt2stdhep = "" 
       }

-- | 
processSetup :: ProcessSetup SM
processSetup = PS {  
    model = SM
  , process = MGProc [] ["p p > t t~"]
  , processBrief = "ttbar" 
  , workname   = "TestTTBar"
  , hashSalt = HashSalt Nothing
  }

-- | 
pset :: ModelParam SM
pset = SMParam

-- | 
rsetup :: RunSetup
rsetup = RS { numevent = 10000
              , machine = LHC7 ATLAS
              , rgrun   = Auto -- Fixed
              , rgscale = 200.0
              , match   = NoMatch
              , cut     = NoCut 
              , pythia  = NoPYTHIA -- RunPYTHIA
              , lhesanitizer = [Replace [(9000201,1000022),(-9000201,1000022)]]
              , pgs     = RunPGS (Cone 0.4, WithTau)
              , uploadhep = NoUploadHEP
              , setnum  = 1
              } 

-- | 
getWSetup :: IO (WorkSetup SM)
getWSetup = WS <$> getScriptSetup <*> pure processSetup <*> pure pset <*> pure rsetup  
               <*> pure (WebDAVRemoteDir "")

main :: IO ()
main = do 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  work =<< getWSetup 

-- | 
work :: (WorkSetup SM) -> IO ()
work wsetup = do 
  r <- flip runReaderT wsetup . runErrorT $ do 
       WS ssetup psetup _ rs _ <- ask                  
       let wb = mcrundir ssetup 
           wn = workname psetup  
       b <- liftIO $ (doesDirectoryExist (wb </> wn))
       when (not b) $ createWorkDir ssetup psetup
       cardPrepare                      
       generateEvents   
       case (lhesanitizer rs, pythia rs) of
         ([], _) -> return ()
         (_:_, RunPYTHIA8) -> return ()
         (_:_, RunPYTHIA) -> do 
           sanitizeLHE
           runPYTHIA
           runPGS           
           runClean         
         (_:_, NoPYTHIA) -> do 
           sanitizeLHE
       cleanHepFiles  
  print r  
  return ()
