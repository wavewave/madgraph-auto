-- {-# LANGUAGE QuasiQuotes #-}

module Main where

import Model
import Machine
import Work
import Fortran 
import UserCut 

-- import SimpleQQ

import System.Posix.Unistd (sleep)

ssetup = SS {
    scriptbase = "/nobackup/iankim/script/madgraph_auto/"
  , mg5base    = "/nobackup/iankim/montecarlo/MG_ME_V4.4.44/MadGraph5_v0_6_1/"
  , workbase   = "/nobackup/iankim/wk/"
  }

ucut = UserCut { 
    uc_metcut    = 15.0 
  , uc_etacutlep = 1.2 
  , uc_etcutlep  = 18.0
  , uc_etacutjet = 2.5
  , uc_etcutjet  = 15.0 
}


processTTBar0or1jet =  
  "\ngenerate P P > t t~  QED=99 @1 \nadd process P P > t t~ J QED=99 @2 \n"

psetup_six_ttbar01j = PS {  
    mversion = MadGraph5
  , model = Six
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "208Six1J"
  }

rsetup p matchtype num = RS { 
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
  , usercut = UserCutDefined
  , pgs     = RunPGS
  , cluster = Cluster "test"
  , setnum  = num
}

sixparamset = [ SixParam  600.0 3.65 ]

psetuplist = [ psetup_six_ttbar01j ]

sets = [ 1 .. 50 ]

sixtasklist =  [ (psetup_six_ttbar01j, rsetup p MLM num) | p <- sixparamset 
                            			         , num <- sets     ]

totaltasklist = sixtasklist 

main = do putStrLn "benchmark models 20110208 sets" 
          putStrLn "models : six "

	  let combinedfunc (psetup,rsetup) = do 
                cardPrepare      ssetup psetup rsetup
                generateEvents   ssetup psetup rsetup
                runHEP2LHE       ssetup psetup rsetup
                runHEPEVT2STDHEP ssetup psetup rsetup
	        runPGS           ssetup psetup rsetup 
                runClean         ssetup psetup rsetup 
                updateBanner     ssetup psetup rsetup ucut
                return () 


          compileFortran ssetup ucut

          mapM_ (createWorkDir ssetup) psetuplist
          sleep 2
          mapM_ combinedfunc totaltasklist 

          