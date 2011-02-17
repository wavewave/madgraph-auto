{-# LANGUAGE PackageImports #-}

module Main where

import "mtl" Control.Monad.Reader 

import Model
import Machine
import UserCut
import Cluster
import SetupType
import Run

-- import SimpleQQ

import System.Posix.Unistd (sleep)


my_ssetup :: ScriptSetup
my_ssetup = SS {
    scriptbase = "/Users/iankim/mac/workspace/ttbar/madgraph_auto/"
  , mg5base    = "/Users/iankim/mac/montecarlo/MG_ME_V4.4.44/MadGraph5_v0_6_1/"
  , workbase   = "/Users/iankim/mac/workspace/ttbar/mc/"
  } 


processTTBar0or1jet :: [Char]
processTTBar0or1jet =  
  "\ngenerate P P > t t~  QED=99 @1 \nadd process P P > t t~ J QED=99 @2 \n"

psetup_wp_ttbar01j = PS {  
    mversion = MadGraph4
  , model = Wp 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "217Wp1J"
  }

psetup_zp_ttbar01j = PS {  
    mversion = MadGraph4
  , model = ZpH 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "217Zp1J"
  }

rsetupGen :: Param -> MatchType -> Int -> RunSetup
rsetupGen p matchtype num = RS { 
    param   = p
  , numevent = 100000
  , machine = TeVatron 
  , rgrun   = Fixed
  , rgscale = 200.0 
  , match   = matchtype
  , cut     = case matchtype of 
      NoMatch -> NoCut 
      MLM     -> DefCut
  , pythia  = case matchtype of 
      NoMatch -> NoPYTHIA
      MLM     -> RunPYTHIA
  , usercut = NoUserCutDef 
  , pgs     = NoPGS
  , setnum  = num
}

my_csetup :: ClusterSetup
my_csetup = CS { cluster = Parallel 3 
 -- Cluster "test" 
}


wpparamset =  [ WpParam 400.0 1.0 ] 

zpparamset = [ ZpHParam 400.0 1.0 ] 

psetuplist = [ psetup_wp_ttbar01j 
	     , psetup_zp_ttbar01j ]

sets = [ 1 .. 50 ]

wptasklist :: [WorkSetup]
wptasklist =  [ WS my_ssetup (psetup_wp_ttbar01j) (rsetupGen p MLM num) my_csetup  
                           | p <- wpparamset 
                           , num <- sets     ]

zptasklist :: [WorkSetup]
zptasklist =  [ WS my_ssetup (psetup_zp_ttbar01j) (rsetupGen p MLM num) my_csetup  
                           | p <- zpparamset 
                           , num <- sets     ]

totaltasklist = wptasklist ++ zptasklist 


main :: IO ()
main = do putStrLn "benchmark models 20110216 sets" 
          putStrLn "models : trip "

	  let cmdSequence = do 
                compileFortran
                cardPrepare                      
                generateEvents   
--                runHEP2LHE       
--                runHEPEVT2STDHEP
--                runPGS            
--                runClean          
--                updateBanner     
                cleanHepFiles
                 
          -- create working directory (only once for each process)
          mapM_ (createWorkDir my_ssetup) psetuplist
          sleep 2
          mapM_ (runReaderT cmdSequence) totaltasklist 

          