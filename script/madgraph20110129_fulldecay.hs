{-# LANGUAGE QuasiQuotes #-}

module Main where

import Model
import Machine
import Work
import SimpleQQ

import System.Posix.Unistd (sleep)

ssetup = SS {
    scriptbase = "/home/wavewave/nfs/workspace/ttbar/madgraph_auto/"
  , mg5base    = "/home/wavewave/nfs/montecarlo/MG_ME_V4.4.44/MadGraph5_v0_6_1/"
  , workbase   = "/home/wavewave/nfs/workspace/ttbar/mc/"
  }


processTTBar01JFull = 
  "\ngenerate P P > t t~ QED=99, (t > b w+, w+ > J J) , (t~ > b~ w-, w- > L- vl~) QED=99 @1 \nadd process P P > t t~ QED=99, ( t > b w+, w+ > L+ vl ), ( t~ > b~ w-, w- > J J ) QED=99  @2 \nadd process P P > t t~ J QED=99, (t > b w+, w+ > J J) , (t~ > b~ w-, w- > L- vl~) QED=99  @3 \nadd process P P > t t~ J QED=99,  ( t > b w+, w+ > L+ vl ) , ( t~ > b~ w-, w- > J J ) QED=99 @4\n"


psetup_wp_ttbar01jFull = PS {  
    mversion = MadGraph4
  , model = Wp 
  , process = processTTBar01JFull
  , processBrief = "ttbar01jsemilep"  
  , workname   = "129WpFull"
  }

rsetup p num = RS { 
    param   = p
  , machine = TeVatron 
  , rgrun   = Fixed
  , rgscale = 200.0 
  , match   = MLM
  , cut     = KCut
  , pythia  = RunPYTHIA
  , pgs     = RunPGS
  , setnum  = num
}


wpparamset = [ WpParam 200.0 1.3 ]



psetuplist = [ psetup_wp_ttbar01jFull ] 

sets = [1] -- [ 3..10 ] 


wptasklist =  [ (psetup_wp_ttbar01jFull, rsetup p num) | p <- wpparamset 
                                                       , num <- sets     ]

totaltasklist = wptasklist 

main = do putStrLn "full decay sample" 
          putStrLn "models : Wp "
          
          mapM_ (createWorkDir ssetup) psetuplist
          sleep 2
          mapM_ (\(psetup,rsetup) -> generateEvents ssetup psetup rsetup) $
            totaltasklist
     