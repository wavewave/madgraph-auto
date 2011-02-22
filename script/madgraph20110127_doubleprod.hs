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

psetup_wp_double = PS {  
    mversion = MadGraph4
  , model = Wp 
  , process = "\ngenerate P P > wp+ wp-\n" 
  , processBrief = "doubleprod"  
  , workname   = "127DWp"
  }

psetup_zp_double = PS {  
    mversion = MadGraph4
  , model = ZpH 
  , process = "\ngenerate P P > zptu zput\n"
  , processBrief = "doubleprod"  
  , workname   = "127DZpH"
  }

psetup_trip_double = PS {  
    mversion = MadGraph5
  , model = Trip 
  , process = "\ngenerate P P > trip trip~\n"
  , processBrief = "doubleprod"  
  , workname   = "127DTrip"
  }

psetup_six_double = PS {  
    mversion = MadGraph5
  , model = Six
  , process = "\ngenerate P P > six six~\n"
  , processBrief = "doubleprod"  
  , workname   = "127DSix"
  }


psetup_wp_single = PS {  
    mversion = MadGraph4
  , model = Wp 
  , process = "\ngenerate P P > wp- t QED=99 @1\nadd process P P > wp+ t~ QED=99 @2\n" 
  , processBrief = "singleprod"  
  , workname   = "127SWp"
  }

psetup_zp_single = PS {  
    mversion = MadGraph4
  , model = ZpH 
  , process = "\ngenerate P P > zptu t~ QED=99 @1\nadd process P P > zput t QED=99 @2\n"
  , processBrief = "singleprod"  
  , workname   = "127SZpH"
  }

psetup_trip_single = PS {  
    mversion = MadGraph5
  , model = Trip 
  , process = "\ngenerate P P > trip~ t~ QED=99 @1\nadd process P P > trip t QED=99 @2\n"
  , processBrief = "singleprod"  
  , workname   = "127STrip"
  }

psetup_six_single = PS {  
    mversion = MadGraph5
  , model = Six
  , process = "\ngenerate P P > six t~ QED=99 @1\nadd process P P > six~ t QED=99 @2\n"
  , processBrief = "singleprod"  
  , workname   = "127SSix"
  }



rsetup p = RS { 
    param   = p
  , machine = LHC7
  , rgrun   = Fixed
  , rgscale = 200.0 
  , match   = NoMatch
  , cut     = NoCut 
  , pythia  = NoPYTHIA
  , pgs     = NoPGS
  , setnum  = 1
}

wpparamset = [ WpParam 200.0 1.0
             , WpParam 300.0 1.0 
             , WpParam 400.0 1.0
             , WpParam 600.0 1.0 ] 

zpparamset = [ ZpHParam 200.0 1.0 
             , ZpHParam 300.0 1.0 
             , ZpHParam 400.0 1.0 
             , ZpHParam 600.0 1.0 ] 
           
tripparamset = [ TripParam M200C1_0
               , TripParam M300C1_0 
               , TripParam M400C1_0
               , TripParam M600C1_0 ] 
             
sixparamset = [ SixParam M200C1_0
              , SixParam M300C1_0 
              , SixParam M400C1_0
              , SixParam M600C1_0 ] 

{- psetuplist = [ psetup_wp_double
             , psetup_zp_double
             , psetup_trip_double
             , psetup_six_double
             , psetup_wp_single
             , psetup_zp_single
             , psetup_trip_single
             , psetup_six_single ] -}

psetuplist = [ psetup_trip_double
             , psetup_six_double
             , psetup_trip_single
             , psetup_six_single ] 

wptasklist = [ (psetup, rsetup p ) | psetup <- [psetup_wp_double, psetup_wp_single]
                                   , p <- wpparamset ]

zptasklist = [ (psetup, rsetup p ) | psetup <- [psetup_zp_double, psetup_zp_single]
                                   , p <- zpparamset ]

triptasklist = [ (psetup, rsetup p ) | psetup <- [psetup_trip_double, psetup_trip_single]
                                   , p <- tripparamset ]

sixtasklist = [ (psetup, rsetup p ) | psetup <- [psetup_six_double, psetup_six_single]
                                   , p <- sixparamset ]

totaltasklist = triptasklist ++ sixtasklist

main = do putStrLn "benchmark models 20110126 sets" 
          putStrLn "models : Wp, ZpH, Trip, Six "
          
          mapM_ (createWorkDir ssetup) psetuplist
          sleep 2
          mapM_ (\(psetup,rsetup) -> generateEvents ssetup psetup rsetup) $
            totaltasklist