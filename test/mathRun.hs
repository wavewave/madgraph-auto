-- {-# LANGUAGE QuasiQuotes #-}

module Main where

import Util
import Model
import Machine
import UserCut
import Cluster
import SetupType
import Mathematica


import System.Posix.Unistd (sleep)

ssetup = SS {
    scriptbase = "/nobackup/iankim/script/madgraph_auto/"
  , mg5base    = "/nobackup/iankim/montecarlo/MG_ME_V4.4.44/MadGraph5_v0_6_1/"
  , workbase   = "/nobackup/iankim/wk/"
  }


ucut :: UserCut
ucut = UserCut { 
    uc_metcut    = 15.0 
  , uc_etacutlep = 1.2 
  , uc_etcutlep  = 18.0
  , uc_etacutjet = 2.5
  , uc_etcutjet  = 15.0 
}

processTTBar0or1jet =  
  "\ngenerate P P > t t~  QED=99 @1 \nadd process P P > t t~ J QED=99 @2 \n"

psetup_zp_ttbar01j = PS {  
    mversion = MadGraph4
  , model = ZpH 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "205ZpH1J"
  }

psetup_wp_ttbar01j = PS {  
    mversion = MadGraph4
  , model = Wp 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "206Wp1J"
  }

psetup_trip_ttbar01j = PS {  
    mversion = MadGraph5
  , model = Trip 
  , process = processTTBar0or1jet 
  , processBrief = "ttbar01j"  
  , workname   = "207Trip1J"
  }


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
  , usercut = UserCutDef ucut
  , pgs     = RunPGS
  , setnum  = num
}

my_csetup = CS { 
  cluster = Cluster "test"
}

zpparamset = [ ZpHParam 300.0 1.41 ]

wpparamset =  [ WpParam 200.0 1.60 ] 

tripparamset = [ TripParam 600.0 4.4 ]

sixparamset = [ SixParam  600.0 3.65 ]

sets = [1..50]

zptasklist =  [ (psetup_zp_ttbar01j, rsetup p MLM num) | p <- zpparamset 
                                                       , num <- sets     ] 

wptasklist =  [ (psetup_wp_ttbar01j, rsetup p MLM num) | p <- wpparamset 
        					       , num <- sets     ]  

triptasklist =  [ (psetup_trip_ttbar01j, rsetup p MLM num) | p <- tripparamset 
                                                           , num <- sets     ]

sixtasklist =  [ (psetup_six_ttbar01j, rsetup p MLM num) | p <- sixparamset 
                            			         , num <- sets     ]


totaltasklist = wptasklist ++ triptasklist ++ sixtasklist  {- ++ zptasklist -}


chisqrcutvalue=100000

main = do putStrLn "mathematica reconstruction 20110206,07,08 set" 
          putStrLn "models : Wp, Trip, Six "

	  let mathematica_analysis chisqrcut (psetup,rsetup) = do 
		let runname = makeRunName psetup rsetup  		
	        putStrLn runname
                let tp = (scriptbase ssetup) ++ "template/" 
                    eventdir = (workbase ssetup)++(workname psetup)++"/Events/"
                    workdir = (scriptbase ssetup) ++ "working/"
                    chameleondir = tp 
                    chameleon = "Chameleon1_02.m"
                    lhcofile = eventdir ++ runname ++ "_pgs_events.lhco"
                    sampleCutEvtsdat = eventdir++runname++"_sampleCutEvts_"++"ChiSqrCut"++ (show chisqrcut) ++ ".dat"
                    sampleRecoEvtsdat = eventdir++runname++"_sampleRecoEvts_"++"ChiSqrCut"++ (show chisqrcut) ++ ".dat"
                    samplePassedEvtsdat = eventdir++runname++"_samplePassedEvts_"++"ChiSqrCut"++(show chisqrcut) ++ ".dat"
                    sampleRecoInfodat = eventdir++runname++"_sampleRecoInfo_"++"ChiSqrCut"++(show chisqrcut) ++".dat"
                    mfile = workdir ++ runname ++ ".m"
                    ofile = workdir ++ runname ++ ".log"
                    pbsfile = workdir ++ runname ++ ".pbs"
                mathematicaSendBatch MSetup { 
                                       ms_chameleondir = chameleondir, 
                                       ms_chameleon    = chameleon, 
                                       ms_lhcofile    = lhcofile,
                                       ms_sampleCutEvtsdat = sampleCutEvtsdat, 
                                       ms_sampleRecoEvtsdat = sampleRecoEvtsdat, 
                                       ms_samplePassedEvtsdat = samplePassedEvtsdat, 
                                       ms_sampleRecoInfodat = sampleRecoInfodat, 
                                       ms_chisqrcut = chisqrcut,
                                       ms_mfile = mfile, 
                                       ms_ofile = ofile,
                                       ms_pbsfile = pbsfile
                                     }
                                     tp workdir
                return () 

	  mapM_ (mathematica_analysis chisqrcutvalue) totaltasklist 


          