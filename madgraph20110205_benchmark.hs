-- {-# LANGUAGE QuasiQuotes #-}

module Main where

import Model
import Machine
import Work
import Fortran 

-- import SimpleQQ

import System.Posix.Unistd (sleep)

ssetup = SS {
    scriptbase = "/nobackup/iankim/script/madgraph_auto/"
  , mg5base    = "/nobackup/iankim/montecarlo/MG_ME_V4.4.44/MadGraph5_v0_6_1/"
  , workbase   = "/nobackup/iankim/wk/"
  }

ucut = UserCut { 
    uc_metcut    = 20.0 
  , uc_etacutlep = 1.0 
  , uc_etcutlep  = 20.0
  , uc_etacutjet = 2.5
  , uc_etcutjet  = 20.0 
}


processTTBar0or1jet =  
  "\ngenerate P P > t t~  QED=99 @1 \nadd process P P > t t~ J QED=99 @2 \n"

psetup_wp_ttbar01j = PS {  
    mversion = MadGraph4
  , model = Wp 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "205Wp1J"
  }

psetup_zp_ttbar01j = PS {  
    mversion = MadGraph4
  , model = ZpH 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "205ZpH1J"
  }

psetup_trip_ttbar01j = PS {  
    mversion = MadGraph5
  , model = Trip 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "205Trip1J"
  }

psetup_six_ttbar01j = PS {  
    mversion = MadGraph5
  , model = Six
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "205Six1J"
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
  , pgs     = NoPGS
  , cluster = Cluster "test"
  , setnum  = num
}


wpparamset = [ WpParam 200.0 (0.85*sqrt 2) ] 
{-             , WpParam 300.0 (1.20*sqrt 2) 
             , WpParam 400.0 (1.50*sqrt 2)
             , WpParam 600.0 (2.00*sqrt 2) ] -}

zpparamset = [ ZpHParam 200.0 (0.70*sqrt 2) 
             , ZpHParam 300.0 (1.00*sqrt 2) 
             , ZpHParam 400.0 (1.30*sqrt 2) 
             , ZpHParam 600.0 (1.70*sqrt 2) ] 
           
tripparamset = [ TripParam 400.0 3.45
               , TripParam 400.0 3.3 
               , TripParam 400.0 3.15
               , TripParam 600.0 4.4  
               , TripParam 600.0 4.2 
               , TripParam 600.0 4.0  ] 
             
sixparamset = [ SixParam  600.0 3.5   
              , SixParam  600.0 3.35 
              , SixParam  600.0 3.2  ]  


psetuplist = [ psetup_wp_ttbar01j ]
--             , psetup_zp_ttbar01j
--             , psetup_trip_ttbar01j
--             , psetup_six_ttbar01j ] 

sets = [1,2] -- [ 3..10 ] 


wptasklist =  [ (psetup_wp_ttbar01j, rsetup p MLM num) | p <- wpparamset 
        					       , num <- sets     ]  
	
zptasklist =  [ (psetup_zp_ttbar01j, rsetup p MLM num) | p <- zpparamset 
                                                       , num <- sets     ] 
                  
triptasklist =  [ (psetup_trip_ttbar01j, rsetup p MLM num) | p <- tripparamset 
                                                           , num <- sets     ]

sixtasklist =  [ (psetup_six_ttbar01j, rsetup p MLM num) | p <- sixparamset 
                                                         , num <- sets     ] 

totaltasklist = wptasklist {- ++ zptasklist ++ triptasklist ++ sixtasklist -}

main = do putStrLn "benchmark models 20110205 sets" 
          putStrLn "models : Wp, ZpH, Trip, Six "
          
          compileFortran ssetup ucut
{-
          mapM_ (createWorkDir ssetup) psetuplist
          sleep 2
          mapM_ (\(psetup,rsetup) -> generateEvents ssetup psetup rsetup) $
            totaltasklist -}
     