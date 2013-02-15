{-# LANGUAGE PackageImports #-}

module Main where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Reader 
import "mtl" Control.Monad.Error
import System.FilePath 
import System.Directory 
import System.Log.Logger
-- 
import HEP.Automation.MadGraph.Model
-- import HEP.Automation.MadGraph.Model.SM
import HEP.Automation.MadGraph.Model.ADMXQLD311
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
import HEP.Storage.WebDAV
-- 
import qualified Paths_madgraph_auto as PMadGraph
import qualified Paths_madgraph_auto_model as PModel

-- |  
getScriptSetup :: IO ScriptSetup
getScriptSetup = do 
  mdldir <- (</> "template") <$> PModel.getDataDir
  rundir <- (</> "template") <$> PMadGraph.getDataDir 
  return $ 
    SS { modeltmpldir = mdldir 
       , runtmpldir = rundir 
       , sandboxdir = "/home/wavewave/repo/workspace/montecarlo/working"
       , mg5base    = "/home/wavewave/repo/ext/MadGraph5_v1_4_8_4/"
       , mcrundir   = "/home/wavewave/repo/workspace/montecarlo/mc/"
       }

-- | 
processSetup :: ProcessSetup ADMXQLD311
processSetup = PS {  
    model = ADMXQLD311
  , process = "\ngenerate P P > t1 t1~ QED=0, t1 > d e+ sxxp~ , t1~ > d~ e- sxxp \n"
    -- "\ngenerate P P > t t~ \n" -- 
  , processBrief = "stoppair_full" 
    -- "ttbar" -- 
  , workname   = "Test13_20130215_ADMXQLD"
  }

-- | 
psets :: [ModelParam ADMXQLD311]
psets = [ ADMXQLD311Param x | x <- [500,1000] ] 
-- [1500] ] 
-- [100] ] 
-- [600,700,800] ]

-- [100,200..1600] ]

-- | 
ucut :: UserCut 
ucut = UserCut { 
    uc_metcut = 15.0
  , uc_etacutlep = 2.7
  , uc_etcutlep = 18.0 
  , uc_etacutjet = 2.7
  , uc_etcutjet = 15.0 
}

-- | 
rsetup p = RS { param = p
            , numevent = 10000
            , machine = LHC7 ATLAS
            , rgrun   = Fixed
            , rgscale = 200.0
            , match   = NoMatch
            , cut     = NoCut 
            , pythia  = RunPYTHIA
            , usercut = NoUserCutDef 
            , lhesanitizer = LHESanitize (Replace [(9000201,1000022),(-9000201,1000022)]) -- NoLHESanitize
                             -- LHESanitize (Elim [9000201]) 
            , pgs     = RunPGS
            , jetalgo = Cone 0.4
            , uploadhep = NoUploadHEP
            , setnum  = 1
            }

-- | 
getWSetup :: [IO (WorkSetup ADMXQLD311)]
getWSetup = [ WS <$> getScriptSetup <*> pure processSetup <*> pure (rsetup p) 
                 <*> pure (CS NoParallel) 
                 <*> pure (WebDAVRemoteDir "") | p <- psets ]

main = do 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)

  mapM_ work =<< sequence getWSetup 

-- | 
-- work p  -- :: IO ()
work wsetup = do -- wsetup <- getWSetup 
            r <- flip runReaderT wsetup . runErrorT $ do 
                 WS ssetup psetup rsetup _ _ <- ask 

                 
                 let wb = mcrundir ssetup 
                     wn = workname psetup  
                 b <- liftIO $ (doesDirectoryExist (wb </> wn))
                 when (not b) $ createWorkDir ssetup psetup
                 cardPrepare                      
                 generateEvents   
                 case (lhesanitizer rsetup,usercut rsetup,pythia rsetup) of
                   (NoLHESanitize, NoUserCutDef,_) -> return ()
                   (NoLHESanitize, UserCutDef _,_) -> do 
                     runHEP2LHE       
                     runHEPEVT2STDHEP 
                     runPGS           
                     runClean         
                     updateBanner   
                   (LHESanitize pid, NoUserCutDef, RunPYTHIA) -> do 
                     sanitizeLHE
                     runPYTHIA
                     runHEP2LHE
                     runPGS           
                     runClean         
                     updateBanner   
                   (LHESanitize pid, NoUserCutDef, NoPYTHIA) -> do 
                     sanitizeLHE
                     updateBanner   
                   (LHESanitize pid, UserCutDef _,RunPYTHIA) -> do 
                     sanitizeLHE
                     runPYTHIA
                     runHEP2LHE       
                     runHEPEVT2STDHEP 
                     runPGS           
                     runClean         
                     updateBanner   
                   (LHESanitize pid, UserCutDef _,NoPYTHIA) -> do 
                     sanitizeLHE
                     updateBanner    
                 cleanHepFiles  
            print r  
            return ()




          {-
          -- create working directory (only once for each process)
          mapM_ (createWorkDir my_ssetup) psetuplist
          sleep 2
          mapM_ (runReaderT cmdSequence) totaltasklist 
          -}
          