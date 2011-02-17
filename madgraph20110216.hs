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

psetup_trip_ttbar01j = PS {  
    mversion = MadGraph5
  , model = Trip 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "216Trip1J"
  }

rsetupGen :: Param -> MatchType -> Int -> RunSetup
rsetupGen p matchtype num = RS { 
    param   = p
  , numevent = 10000
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


tripparamset = [ TripParam m g | 
                      m <- [ 300, 400, 500, 600, 700, 800, 900, 1000 ], 
                      g <- [ 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5 ] 
               ]

psetuplist = [ psetup_trip_ttbar01j ]

sets = [ 1 ]

triptasklist :: [WorkSetup]
triptasklist =  [ WS my_ssetup (psetup_trip_ttbar01j) (rsetupGen p MLM num) my_csetup  
                           | p <- tripparamset 
                           , num <- sets     ]

totaltasklist = triptasklist 


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

          