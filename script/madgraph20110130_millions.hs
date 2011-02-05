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


processTTBar = 
  "\ngenerate P P > t t~  QED=99 @1 \n"

processTTBar0or1jet =  
  "\ngenerate P P > t t~  QED=99 @1 \nadd process P P > t t~ J QED=99 @2 \n"

psetup_wp_ttbar = PS {  
    mversion = MadGraph4
  , model = Wp 
  , process = processTTBar
  , processBrief = "ttbar"  
  , workname   = "126Wp0J"
  }

psetup_zp_ttbar = PS {  
    mversion = MadGraph4
  , model = ZpH 
  , process = processTTBar
  , processBrief = "ttbar"  
  , workname   = "126ZpH0J"
  }

psetup_trip_ttbar = PS {  
    mversion = MadGraph5
  , model = Trip 
  , process = processTTBar
  , processBrief = "ttbar"  
  , workname   = "126Trip0J"
  }

psetup_six_ttbar = PS {  
    mversion = MadGraph5
  , model = Six
  , process = processTTBar
  , processBrief = "ttbar"  
  , workname   = "126Six0J"
  }

psetup_wp_ttbar01j = PS {  
    mversion = MadGraph4
  , model = Wp 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "126Wp1J"
  }

psetup_zp_ttbar01j = PS {  
    mversion = MadGraph4
  , model = ZpH 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "126ZpH1J"
  }

psetup_trip_ttbar01j = PS {  
    mversion = MadGraph5
  , model = Trip 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "126Trip1J"
  }

psetup_six_ttbar01j = PS {  
    mversion = MadGraph5
  , model = Six
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "126Six1J"
  }


rsetup p matchtype num = RS { 
    param   = p
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
  , setnum  = num
}


wpparamset = [ WpParam 200.0 1.3 ] 
                         
{- zpparamset = [ ZpHParam 200.0 0.90
             , ZpHParam 200.0 0.95 
             , ZpHParam 300.0 1.3 
             , ZpHParam 300.0 1.2 
             , ZpHParam 400.0 1.75 
             , ZpHParam 400.0 1.65 
             , ZpHParam 600.0 2.3 
             , ZpHParam 600.0 2.2 ] -}


{-
wpparamset = [ WpParam 200.0 (0.85*sqrt 2)
             , WpParam 300.0 (1.20*sqrt 2) 
             , WpParam 400.0 (1.50*sqrt 2)
             , WpParam 600.0 (2.00*sqrt 2) ] 

zpparamset = [ ZpHParam 200.0 (0.70*sqrt 2) 
             , ZpHParam 300.0 (1.00*sqrt 2) 
             , ZpHParam 400.0 (1.30*sqrt 2) 
             , ZpHParam 600.0 (1.70*sqrt 2) ] 
           
tripparamset = [ TripParam M400C3_45
               , TripParam M400C3_3 
               , TripParam M400C3_15
               , TripParam M600C4_4 
               , TripParam M600C4_2 
               , TripParam M600C4_0  ] 
             
sixparamset = [ SixParam  M600C3_5   
              , SixParam  M600C3_35 
              , SixParam  M600C3_2  ]  -}

{-
tripparamset = [ TripParam 400 3.0
               , TripParam 400 2.75
               , TripParam 400 2.5
               , TripParam 600 3.75
               , TripParam 600 3.5 
               , TripParam 600 3.25 ] 
             
sixparamset = [ SixParam 600 3.0  
              , SixParam 600 2.75 
              , SixParam 600 2.5 ]  -}


psetuplist = [ psetup_wp_ttbar
             , psetup_zp_ttbar
             , psetup_trip_ttbar
             , psetup_six_ttbar
             , psetup_wp_ttbar01j
             , psetup_zp_ttbar01j
             , psetup_trip_ttbar01j
             , psetup_six_ttbar01j ] 

sets = [11..100] -- [ 3..10 ] 


wptasklist =  [ (psetup_wp_ttbar01j, rsetup p MLM num) | p <- wpparamset 
                                                       , num <- sets     ]  
              
{-
zptasklist =  [ (psetup_zp_ttbar, rsetup p NoMatch num) | p <- zpparamset 
                                                        , num <- sets     ]
           ++ [ (psetup_zp_ttbar01j, rsetup p MLM num) | p <- zpparamset 
                                                           , num <- sets     ] -}
{-                  
triptasklist =  [ (psetup_trip_ttbar, rsetup p NoMatch num) | p <- tripparamset 
                                                            , num <- sets     ]
             ++ [ (psetup_trip_ttbar01j, rsetup p MLM num) | p <- tripparamset 
                                                               , num <- sets     ]

sixtasklist =  [ (psetup_six_ttbar, rsetup p NoMatch num) | p <- sixparamset 
                                                          , num <- sets     ]
            ++ [ (psetup_six_ttbar01j, rsetup p MLM num) | p <- sixparamset 
                                                             , num <- sets     ] 
-}
totaltasklist = wptasklist {- ++ zptasklist ++ triptasklist ++ sixtasklist -}

main = do putStrLn "benchmark models 20110128 sets" 
          putStrLn "models : Wp, ZpH, Trip, Six "
          
--          mapM_ (createWorkDir ssetup) psetuplist
          sleep 2
          mapM_ (\(psetup,rsetup) -> generateEvents ssetup psetup rsetup) $
            totaltasklist
     