{-# LANGUAGE ScopedTypeVariables #-}

module Run where

import System.Process
import System.Directory
import System.Posix.Unistd (sleep)
import System.Posix.Env 


import Control.Monad.Reader

import Util
import Model 
import Machine
import Cluster
import UserCut
import SetupType


compileFortran :: WorkIO ()
compileFortran = do 
  WS ssetup _ rsetup _ <- ask   
  liftIO $ 
    case (usercut rsetup) of 
      NoUserCutDef -> return () 
      UserCutDef uc -> do 
        putStrLn $ "set up fortran program" 
        let workingdir  = scriptbase ssetup ++ "working/"
            templatedir = scriptbase ssetup ++ "template/"

        let existThenRemoveForAny x = existThenRemove (workingdir ++ x)
            cpFrmTmpl2Working x = copyFile (templatedir ++ x) (workingdir ++ x)
	
        let filelistNoTemplate   =  [ "getjet.f" 
                                    , "hepevt2stdhep.f"
                                    , "pgs_ranmar.f" 
                                    , "pythia.f"
                                    , "stdhep_print.f"
                                    , "pgs.inc" 
                                    , "compile.sh" 
                                    , "ktclusdble.f"
                                    , "ME2pythia.f" ]
        -- erase previous run 
        mapM_ existThenRemoveForAny  (("hep2lhe.f") : filelistNoTemplate)

        -- setup new hep2lhe.f with a given user cut 
        hep2lhe    <- hep2lheSetup templatedir uc
        writeFile (workingdir ++ "hep2lhe.f") hep2lhe

        -- copy files and compile
        mapM_ cpFrmTmpl2Working filelistNoTemplate 
        setCurrentDirectory workingdir
        readProcess ("./compile.sh") [] "" 

        return ()


createWorkDir :: ScriptSetup -> ProcessSetup -> IO ()
createWorkDir ssetup psetup = do 
  putStrLn $ "set up a working directory" 
  let tempdir = scriptbase ssetup ++ "working/"
      processfilecontent = makeProcessFile (model psetup) (mversion psetup) (process psetup) (workname psetup)
  writeFile (tempdir ++ "proc_card_mg5.dat") processfilecontent

  setCurrentDirectory (mg5base ssetup)
  readProcess ("bin/mg5") [tempdir ++ "proc_card_mg5.dat"] ""
  putStrLn "Wait Two Seconds"  
  sleep 2
  putStrLn $ "moving directory" 
             ++ (mg5base ssetup ++ workname psetup) 
             ++ " to " 
             ++ (workbase ssetup ++ workname psetup) 
  renameDirectory (mg5base ssetup ++ workname psetup) (workbase ssetup ++ workname psetup) 
  return ()

cardPrepare :: WorkIO () 
cardPrepare = do 
  WS ssetup psetup rsetup _ <- ask 
  liftIO $ do 
    let taskname = makeRunName psetup rsetup 
    putStrLn $ "prepare for cards for " ++ taskname
  
  
    let carddir = workbase ssetup ++ workname psetup ++ "/Cards/"
        tpath   = scriptbase ssetup ++ "template/"
  
    -- erase previous run 
    existThenRemove (carddir ++ "param_card.dat") 
    existThenRemove (carddir ++ "run_card.dat") 
    existThenRemove (carddir ++ "pythia_card.dat") 
    existThenRemove (carddir ++ "pgs_card.dat")
  
    paramcard  <- paramCardSetup 
                    tpath
                    (model psetup)
                    (param rsetup)
  
    runcard    <- runCardSetup 
                    tpath
                    (machine rsetup) 
                    (cut     rsetup) 
                    (match   rsetup) 
                    (rgrun   rsetup) 
                    (rgscale rsetup) 
		    (numevent rsetup) 
                  
    pythiacard <- pythiaCardSetup 
                    tpath
                    (match   rsetup)
                    (pythia  rsetup) 
                  
    pgscard    <- pgsCardSetup
                    tpath
                    (machine rsetup)
                    (pgs     rsetup) 
                  
    writeFile (carddir ++ "param_card.dat") paramcard
    writeFile (carddir ++ "run_card.dat")   runcard

    case pythiacard of 
      Nothing -> return () 
      Just str -> writeFile (carddir ++ "pythia_card.dat") str
    
    case pgscard  of 
      Nothing -> return () 
      Just str -> case usercut rsetup of 
        NoUserCutDef  -> writeFile (carddir ++ "pgs_card.dat") str
        UserCutDef _  -> writeFile (carddir ++ "pgs_card.dat.user") str


generateEvents :: WorkIO () 
generateEvents = do 
  WS ssetup psetup rsetup csetup <- ask
  liftIO $ do 
    let taskname = makeRunName psetup rsetup 
  
    putStrLn $ "generating event for " ++ taskname
  
    setCurrentDirectory (workbase ssetup ++ workname psetup)
    case cluster csetup of
      NoParallel -> readProcess ("bin/generate_events") ["0", taskname] ""
      Parallel ncore -> readProcess ("bin/generate_events") ["2", show ncore, taskname] ""
      Cluster cname -> readProcess ("bin/generate_events") ["1", cname, taskname] "" 
    return ()

runHEP2LHE :: WorkIO () 
runHEP2LHE = do
  WS ssetup psetup rsetup _ <- ask 
  liftIO $ do
    let eventdir = workbase ssetup ++ workname psetup ++ "/Events/" 
        workingdir = scriptbase ssetup ++ "working/"
        taskname = makeRunName psetup rsetup 
        hepfilename = taskname++"_pythia_events.hep"
        hepevtfilename = "afterusercut.hepevt"  
    setCurrentDirectory eventdir
  
    b <- doesFileExist hepfilename 
    if b 
      then do 
        putStrLn "Start hep2lhe"
        readProcess (workingdir++"hep2lhe.iw") [hepfilename,hepevtfilename] "" 
      else error "ERROR pythia result does not exist"  
    return () 

runHEPEVT2STDHEP :: WorkIO () 
runHEPEVT2STDHEP = do
  WS ssetup psetup _ _ <- ask 
  liftIO $ do 
    let eventdir = workbase ssetup ++ workname psetup ++ "/Events/" 
        workingdir = scriptbase ssetup ++ "working/"
        hepevtfilename = "afterusercut.hepevt"  
        stdhepfilename = "afterusercut.stdhep" 
  
    setCurrentDirectory eventdir
  
    b <- doesFileExist hepevtfilename 
    if b 
      then do 
        putStrLn "Start hepevt2stdhep"
        readProcess (workingdir++"hepevt2stdhep.iw") [hepevtfilename,stdhepfilename] "" 
      else error "ERROR pythia result does not exist"  
    return () 

runPGS :: WorkIO () 
runPGS = do
  WS ssetup psetup _ _ <- ask 
  liftIO $ do
    let eventdir = workbase ssetup ++ workname psetup ++ "/Events/" 
        pgsdir = workbase ssetup ++ workname psetup ++ "/../pythia-pgs/src/"
        carddir = workbase ssetup ++ workname psetup ++ "/Cards/"
        stdhepfilename = "afterusercut.stdhep" 
        uncleanedfilename = "pgs_uncleaned.lhco"
    setCurrentDirectory eventdir
    renameFile (carddir++"pgs_card.dat.user") (carddir++"pgs_card.dat")
    sleep 1
    b <- doesFileExist stdhepfilename 
    if b 
      then do 
        putStrLn "Start pgs"
        putEnv  $ "PDG_MASS_TBL=" ++ pgsdir ++ "mass_width_2004.mc "
        readProcess (pgsdir++"pgs") ["--stdhep",stdhepfilename,"--nev","0","--detector","../Cards/pgs_card.dat",uncleanedfilename] "" 
      else error "ERROR pythia result does not exist"  
    return () 

runClean :: WorkIO () 
runClean = do
  WS ssetup psetup rsetup _ <- ask 
  liftIO $ do
    let eventdir = workbase ssetup ++ workname psetup ++ "/Events/" 
        pgsdir = workbase ssetup ++ workname psetup ++ "/../pythia-pgs/src/"
        taskname = makeRunName psetup rsetup 
        hepfilename = taskname++"_pythia_events.hep"
        hepevtfilename = "afterusercut.hepevt"  
        stdhepfilename = "afterusercut.stdhep" 
        uncleanedfilename = "pgs_uncleaned.lhco"
        cleanedfilename = "pgs_cleaned.lhco"
        finallhco = taskname ++ "_pgs_events.lhco"
        existThenRemoveForAny x = existThenRemove (eventdir ++ x)
        clean_event_directory = 
          mapM_ existThenRemoveForAny  [ hepfilename
                                       , hepevtfilename
                                       , stdhepfilename
                                       , uncleanedfilename
                                       , cleanedfilename ]
    setCurrentDirectory eventdir
    b <- doesFileExist stdhepfilename 
    if b 
      then do 
        putStrLn "Start clean_output"
        readProcess (pgsdir++"clean_output") [ "-muon", uncleanedfilename, cleanedfilename ] "" 
        renameFile (eventdir++cleanedfilename) (eventdir++finallhco)
        sleep 10
        clean_event_directory
      else error "ERROR pythia result does not exist"  
    return () 

updateBanner :: WorkIO () 
updateBanner = do
  WS ssetup psetup rsetup _ <- ask 
  liftIO $ do 
    case (usercut rsetup) of 
      NoUserCutDef -> return () 
      UserCutDef uc -> do  
        let eventdir = workbase ssetup ++ workname psetup ++ "/Events/" 
            taskname = makeRunName psetup rsetup 
            carddir = workbase ssetup ++ workname psetup ++ "/Cards/"
            bannerfilename = taskname ++ "_banner.txt"
            newbannerfilename = taskname ++ "_newbanner.txt"
            usercutcontent = prettyprintUserCut uc
        setCurrentDirectory eventdir
        bannerstr  <- readFile (eventdir ++ bannerfilename)
        pgscardstr <- readFile (carddir ++ "pgs_card.dat")  
        let newbannerstr = bannerstr ++ usercutcontent ++ pgscardstr
        writeFile (eventdir ++ newbannerfilename) newbannerstr 
