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

processTTBar0or1jSemiTau :: [Char]
processTTBar0or1jSemiTau = 
  "\ngenerate P P > t t~ QED=99, (t > b w+, w+ > J J), (t~ > b~ w-, w- > ta- vt~) QED=99 @1\nadd process P P > t t~ QED=99,  ( t > b w+, w+ > ta+ vt ),  ( t~ > b~ w-, w- > J J ) QED=99 @2\nadd process P P > t t~ J QED=99, (t > b w+, w+ > J J), (t~ > b~ w-, w- > ta- vt~) QED=99 @3\nadd process P P > t t~ J QED=99,  ( t > b w+, w+ > ta+ vt ),  ( t~ > b~ w-, w- > J J ) QED=99 @4 \n"


psetup_wp_ttbar01j_semitau = PS {  
    mversion = MadGraph4
  , model = Wp 
  , process = processTTBar0or1jSemiTau
  , processBrief = "ttbar01jsemitau"  
  , workname   = "217Wp1J"
  }

psetup_zp_ttbar01j_semitau = PS {  
    mversion = MadGraph4
  , model = ZpH 
  , process = processTTBar0or1jSemiTau
  , processBrief = "ttbar01jsemitau"  
  , workname   = "217Zp1J"
  }

psetup_trip_ttbar01j_semitau = PS {  
    mversion = MadGraph5
  , model = Trip 
  , process = processTTBar0or1jSemiTau
  , processBrief = "ttbar01jsemitau"  
  , workname   = "217Trip1J"
  }

psetup_six_ttbar01j_semitau = PS {  
    mversion = MadGraph5
  , model = Six
  , process = processTTBar0or1jSemiTau
  , processBrief = "ttbar01jsemitau"  
  , workname   = "217Six1J"
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
  , pgs     = RunPGS
  , setnum  = num
}

my_csetup :: ClusterSetup
my_csetup = CS { cluster = Parallel 3 
 -- Cluster "test" 
}


wpparamset = [ WpParam 200.0 1.60 ] 

zpparamset = [ ZpHParam 300.0 1.41 ]

tripparamset = [ TripParam 600.0 4.4 ]

sixparamset = [ SixParam  600.0 3.65 ]


psetuplist = [ psetup_wp_ttbar01j_semitau 
	     , psetup_zp_ttbar01j_semitau
	     , psetup_trip_ttbar01j_semitau
	     , psetup_six_ttbar01j_semitau  ]

sets = [ 1 ]

wptasklist :: [WorkSetup]
wptasklist =  [ WS my_ssetup (psetup_wp_ttbar01j_semitau) (rsetupGen p MLM num) my_csetup  
                           | p <- wpparamset 
                           , num <- sets     ]

zptasklist :: [WorkSetup]
zptasklist =  [ WS my_ssetup (psetup_zp_ttbar01j_semitau) (rsetupGen p MLM num) my_csetup  
                           | p <- zpparamset 
                           , num <- sets     ]

triptasklist :: [WorkSetup]
triptasklist =  [ WS my_ssetup (psetup_trip_ttbar01j_semitau) (rsetupGen p MLM num) my_csetup  
                           | p <- tripparamset 
                           , num <- sets     ]

sixtasklist :: [WorkSetup]
sixtasklist =  [ WS my_ssetup (psetup_six_ttbar01j_semitau) (rsetupGen p MLM num) my_csetup  
                           | p <- sixparamset 
                           , num <- sets     ]


totaltasklist = wptasklist ++ zptasklist ++ triptasklist ++ sixtasklist


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

          