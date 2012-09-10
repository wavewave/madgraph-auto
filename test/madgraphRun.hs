module Main where

import Control.Monad.Reader 
import Control.Monad.Error

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Model.SM
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.UserCut

import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
import HEP.Storage.WebDAV

-- |  
scriptsetup :: ScriptSetup
scriptsetup = 
  SS { modeltmpldir = "/home/wavewave/repo/src/madgraph-auto-model/template/"
     , runtmpldir = "/home/wavewave/repo/src/madgraph-auto/template"
     , sandboxdir = "/home/wavewave/repo/workspace/montecarlo/working"
     , mg5base    = "/home/wavewave/repo/ext/MadGraph5_v1_4_8_4/"
     , mcrundir   = "/home/wavewave/repo/workspace/montecarlo/mc/"
     }

-- | 
processSetup :: ProcessSetup SM
processSetup = PS {  
    model = SM
  , process = "\ngenerate P P > t t~ QED=99\n"
  , processBrief = "TTBar" 
  , workname   = "Test_20120727"
  }

-- | 
pset :: ModelParam SM
pset = SMParam

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
rsetup = RS { param = SMParam 
            , numevent = 10000
            , machine = TeVatron
            , rgrun   = Fixed
            , rgscale = 200.0
            , match   = NoMatch
            , cut     = NoCut 
            , pythia  = NoPYTHIA
            , usercut = NoUserCutDef 
            , lhesanitizer = NoLHESanitize 
            , pgs     = NoPGS
            , jetalgo = Cone 0.4
            , uploadhep = NoUploadHEP
            , setnum  = 1
            }

-- | 
wsetup = WS scriptsetup processSetup rsetup (CS NoParallel) (WebDAVRemoteDir "")

-- | 
main :: IO ()
main = do putStrLn "models : sm "
          r <- flip runReaderT wsetup . runErrorT $ do 
                 WS ssetup psetup _ _ _ <- ask 
                 createWorkDir ssetup psetup
                 cardPrepare                      
                 generateEvents   
          print r 
          return ()


{-                runHEP2LHE       
                runHEPEVT2STDHEP 
	        runPGS            
                runClean          
                updateBanner -}
          



          {-
          -- create working directory (only once for each process)
          mapM_ (createWorkDir my_ssetup) psetuplist
          sleep 2
          mapM_ (runReaderT cmdSequence) totaltasklist 
          -}
          