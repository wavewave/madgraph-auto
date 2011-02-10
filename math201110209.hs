-- {-# LANGUAGE QuasiQuotes #-}

module Main where

import Model
import Machine
import Work
import Mathematica


import System.Posix.Unistd (sleep)

ssetup = SS {
    scriptbase = "/nobackup/iankim/script/madgraph_auto/"
  , mg5base    = "/nobackup/iankim/montecarlo/MG_ME_V4.4.44/MadGraph5_v0_6_1/"
  , workbase   = "/nobackup/iankim/wk/"
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

zpparamset = [ ZpHParam 300.0 1.41 ]

sixparamset = [ SixParam  600.0 3.65 ]


psetuplist = [ psetup_zp_ttbar01j ]

sets = [ 1..50 ]

zptasklist =  [ (psetup_zp_ttbar01j, rsetup p MLM num) | p <- zpparamset 
                                                       , num <- sets     ] 


sixtasklist =  [ (psetup_six_ttbar01j, rsetup p MLM num) | p <- sixparamset 
                            			         , num <- sets     ]

totaltasklist = zptasklist 

main = do putStrLn "mathematica reconstruction 20110205 set" 
          putStrLn "models : Zp "

	  let mathematica_analysis (psetup,rsetup) = do 
		let runname = makeRunName psetup rsetup  		
	        putStrLn runname
                let tp = (scriptbase ssetup) ++ "template/" 
                    eventdir = (workbase ssetup)++(workname psetup)++"/Events/"
                    workdir = (scriptbase ssetup) ++ "working/"
                    chameleondir = tp 
                    chameleon = "Chameleon1_02.m"
                    lhcofile = eventdir ++ runname ++ "_pgs_events.lhco"
                    sampleCutEvtsdat = eventdir++runname++"_sampleCutEvts.dat"
                    sampleRecoEvtsdat = eventdir++runname++"_sampleRecoEvts.dat"
                    samplePassedEvtsdat = eventdir++runname++"_samplePassedEvts.dat"
                    sampleRecoInfodat = eventdir++runname++"_sampleRecoInfo.dat"
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
                                       ms_mfile = mfile, 
                                       ms_ofile = ofile,
                                       ms_pbsfile = pbsfile
                                     }
                                     tp workdir
                return () 

	  mapM_ mathematica_analysis totaltasklist 


          