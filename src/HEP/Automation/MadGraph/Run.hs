{-# LANGUAGE ScopedTypeVariables, PackageImports #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.Run 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- main run script for madgraph/pythia/pgs  
--
-----------------------------------------------------------------------------

module HEP.Automation.MadGraph.Run where

-- from others 
import Control.Concurrent
import Control.Monad.Error
import Control.Monad.Reader
import System.Directory
import System.FilePath 
import System.Posix.Unistd (sleep)
import System.Posix.Files
import System.Posix.Env 
import System.Process
import Text.StringTemplate
-- from hep-platform
import HEP.Automation.MadGraph.LHESanitizer.Parse 
import Text.StringTemplate.Helpers
-- from this package
import HEP.Automation.MadGraph.Log
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.Model 
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.Util

-- | 
compileshSetup :: ScriptSetup -> IO String 
compileshSetup ssetup = do
  let mg4base = mg5base ssetup </> ".."
  templates <- directoryGroup (runtmpldir ssetup)      
  return $ (renderTemplateGroup 
              templates 
              [ ("mgfourbase", mg4base) ]
              "compile.sh") ++ "\n\n\n"

-- | 
compileFortran :: (Model a) => WorkIO a ()
compileFortran = do 
  WS ssetup _ rsetup _ _ <- ask   
  case (usercut rsetup) of 
    NoUserCutDef -> return () 
    UserCutDef uc -> do 
      debugMsgDef "set up fortran program" 
      let existThenRemoveForAny x = existThenRemove (sandboxdir ssetup </> x)
          cpFrmTmpl2Working x = copyFile (runtmpldir ssetup </> x) 
                                         (sandboxdir ssetup </> x)
      let filelistNoTemplate   =  [ "getjet.f" 
                                  , "hepevt2stdhep.f"
                                  , "pgs_ranmar.f" 
                                  , "pythia.f"
                                  , "stdhep_print.f"
                                  , "pgs.inc" 
                                  , "ktclusdble.f"
                                  , "ME2pythia.f"
                                  ]
      -- erase previous run 
      mapM_ existThenRemoveForAny  ("compile.sh" :"hep2lhe.f" : filelistNoTemplate)
      -- setup new hep2lhe.f with a given user cut 
      hep2lhe <- liftIO $ hep2lheSetup (runtmpldir ssetup) uc
      liftIO $ writeFile (sandboxdir ssetup </> "hep2lhe.f") hep2lhe
      -- setup new compile.sh according to system configuration.
      compilesh  <- liftIO $ compileshSetup ssetup 
      liftIO $ writeFile (sandboxdir ssetup </> "compile.sh") compilesh
      -- copy files and compile
      liftIO $ mapM_ cpFrmTmpl2Working filelistNoTemplate 
      liftIO $ setCurrentDirectory (sandboxdir ssetup)
      checkFile (sandboxdir ssetup </> "compile.sh") 10 
      liftIO $ threadDelay 1000000
      workIOReadProcessWithExitCode "sh" ["./compile.sh"] "" 
      return ()

-- | Creating working directory. 
--   Working directory is an autonomous directory of a single madgraph setup
createWorkDir :: (Model a) => ScriptSetup -> ProcessSetup a -> WorkIO a ()
createWorkDir ssetup psetup = do 
  debugMsgDef "set up a working directory" 
  let processfilecontent = makeProcessFile (model psetup) (process psetup) (workname psetup)
  liftIO $ writeFile (sandboxdir ssetup </> "proc_card_mg5.dat") processfilecontent
  checkFile (sandboxdir ssetup </> "proc_card_mg5.dat") 10 
  liftIO $ setCurrentDirectory (mg5base ssetup)
  workIOReadProcessWithExitCode ("bin/mg5") [sandboxdir ssetup </> "proc_card_mg5.dat"] ""
  checkDirectory (mg5base ssetup </> workname psetup) 10
  checkDirectory (mg5base ssetup </> workname psetup </> "SubProcesses") 10
  debugMsgDef $ "moving directory" 
                 ++ (mg5base ssetup </> workname psetup) 
                 ++ " to " 
                 ++ (mcrundir ssetup </> workname psetup) 
  liftIO $ renameDirectory (mg5base ssetup </> workname psetup) (mcrundir ssetup </> workname psetup) 
  return () 

-- | Get a path for working directory
getWorkDir :: (Model a) => WorkIO a FilePath   
getWorkDir = do 
  WS ssetup psetup _rsetup csetup _ <- ask   
  case cluster csetup of 
    Cluster _ cluname -> return $ mcrundir ssetup </> cluname 
    _                 -> return $ mcrundir ssetup </> workname psetup

-- | prepare for cards: param_card.dat, run_card.dat, pythia_card.dat 
--   and pgs_card.dat. Depending on UserDefinedCut or LHESanitize, 
--   pythia_card.dat.sanitize and/or pgs_card.dat.user is created. 
cardPrepare :: (Model a) => WorkIO a () 
cardPrepare = do 
  WS ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  let taskname = makeRunName psetup rsetup 
  let carddir = wdir </> "Cards"
  debugMsgDef $ "prepare for cards for " ++ taskname
  checkDirectory wdir 10
  checkDirectory (wdir </> "SubProcesses") 10
  checkDirectory carddir 10   
  -- erase previous run 
  existThenRemove (carddir </> "mg5_configuration.txt")
  existThenRemove (carddir </> "param_card.dat") 
  existThenRemove (carddir </> "run_card.dat") 
  existThenRemove (carddir </> "pythia_card.dat") 
  existThenRemove (carddir </> "pythia_card.dat.sanitize") 
  existThenRemove (carddir </> "pgs_card.dat")
  existThenRemove (carddir </> "pgs_card.dat.user")
  -- 
  liftIO $ copyFile (runtmpldir ssetup </> "mg5_configuration.txt" ) (carddir </> "me5_configuration.txt" )
  -- 
  paramcard  <- liftIO $ paramCardSetup 
                           (modeltmpldir ssetup)
                           (model psetup)
                           (param rsetup)
  runcard    <- liftIO $ runCardSetup 
                           (runtmpldir ssetup)
                           (machine rsetup) 
                           (cut     rsetup) 
                           (match   rsetup) 
                           (rgrun   rsetup) 
                           (rgscale rsetup) 
		           (numevent rsetup) 
                           (setnum rsetup)
  pythiacard <- liftIO $ pythiaCardSetup 
                           (runtmpldir ssetup)
                           (match   rsetup)
                           (pythia  rsetup) 
  pgscard    <- liftIO $ pgsCardSetup
                           (runtmpldir ssetup)
                           (machine rsetup)
                           (pgs     rsetup) 
                           (jetalgo rsetup)
  --                 
  liftIO $ writeFile (carddir </> "param_card.dat") paramcard
  liftIO $ writeFile (carddir </> "run_card.dat")   runcard
  -- 
  case pythiacard of 
    Nothing  -> return () 
    Just str -> 
      case lhesanitizer rsetup of 
        NoLHESanitize -> liftIO $ writeFile (carddir </> "pythia_card.dat") str
        LHESanitize _ -> liftIO $ writeFile (carddir </> "pythia_card.dat.sanitize") str 
  --   
  case pgscard  of 
    Nothing  -> return () 
    Just str -> case (usercut rsetup,lhesanitizer rsetup) of 
      (NoUserCutDef,NoLHESanitize) -> 
        liftIO $ writeFile (carddir </> "pgs_card.dat") str
      (UserCutDef _,NoLHESanitize) -> 
        liftIO $ writeFile (carddir </> "pgs_card.dat.user") str
      (_,LHESanitize _) -> 
        liftIO $ writeFile (carddir </> "pgs_card.dat.user") str 
  -- 
  case pgs rsetup of 
    RunPGSNoTau -> do 
      liftIO $ copyFile (runtmpldir ssetup </> "run_pgs_notau" ) (mcrundir ssetup </> workname psetup </> "bin" </> "run_pgs" )
      liftIO $ setFileMode (mcrundir ssetup </> workname psetup </> "bin" </> "run_pgs") 0o755 
        
    _ -> return () 
  return () 

-- | 
generateEvents :: (Model a) => WorkIO a () 
generateEvents = do 
  WS _ssetup psetup rsetup csetup _ <- ask
  wdir <- getWorkDir
  let taskname = makeRunName psetup rsetup 
  debugMsgDef $ "generating event for " ++ taskname
  liftIO $ setCurrentDirectory wdir 
  checkFile (wdir </> "Cards/run_card.dat") 10
  checkFile (wdir </> "Cards/param_card.dat") 10
  -- 
  case (pythia rsetup,lhesanitizer rsetup) of 
    (RunPYTHIA,NoLHESanitize) -> checkFile (wdir </> "Cards/pythia_card.dat") 10
    (RunPYTHIA,LHESanitize _) -> checkFile (wdir </> "Cards/pythia_card.dat.sanitize") 10
    (NoPYTHIA,_) -> return () 
  -- 
  case lhesanitizer rsetup of 
    NoLHESanitize -> 
      case (pgs rsetup, usercut rsetup)  of 
        (RunPGS,NoUserCutDef)      -> checkFile (wdir </> "Cards/pgs_card.dat") 10
        (RunPGSNoTau,NoUserCutDef) -> checkFile (wdir </> "Cards/pgs_card.dat") 10      
        (RunPGS,UserCutDef _)      -> checkFile (wdir </> "Cards/pgs_card.dat.user") 10
        (RunPGSNoTau,UserCutDef _) -> checkFile (wdir </> "Cards/pgs_card.dat.user") 10
        (NoPGS,_)  -> return () 
    LHESanitize _ -> 
      case pgs rsetup of 
        NoPGS -> return ()
        _ -> checkFile (wdir </> "Cards/pgs_card.dat.user") 10
  --  
  case cluster csetup of
    NoParallel     -> workIOReadProcessWithExitCode ("bin/generate_events") ["0", taskname] ""
    Parallel ncore -> workIOReadProcessWithExitCode ("bin/generate_events") ["2", show ncore, taskname] ""
    Cluster _ _ -> undefined 
  -- this is because madgraph-5-1.4 changes the file location. 
  let eventdir = wdir </> "Events" 
      unweightedevtfilename = taskname ++ "_unweighted_events.lhe" 
      rawunweightedevtfilename = "unweighted_events.lhe"
  liftIO $ setCurrentDirectory eventdir  
  checkFile (eventdir </> taskname </> rawunweightedevtfilename <.> "gz") 10 
  liftIO $ renameFile (eventdir</>taskname</>rawunweightedevtfilename<.>"gz") 
                      (eventdir</>unweightedevtfilename<.>"gz")
  return ()

-- | 
sanitizeLHE :: (Model a) => WorkIO a () 
sanitizeLHE = do 
  WS _ssetup psetup rsetup _csetup _storage <- ask 
  debugMsgDef "Start sanitizeLHE"
  case lhesanitizer rsetup of 
    NoLHESanitize -> throwError "ERROR: why did you call me? I am in sanitizeLHEFile." 
    LHESanitize pids -> do 
      wdir <- getWorkDir
      let eventdir = wdir </> "Events" 
          taskname = makeRunName psetup rsetup 
          unweightedevtfilename = taskname ++ "_unweighted_events.lhe" 
          rawunweightedevtfilename = "unweighted_events.lhe"

      liftIO $ setCurrentDirectory eventdir
      checkFile (eventdir </> unweightedevtfilename <.> "gz") 10 

      debugMsgDef $ (eventdir </> unweightedevtfilename <.> "gz")

      liftIO $ system ("gunzip -f " ++ unweightedevtfilename <.> "gz") 
      checkFile (eventdir </> unweightedevtfilename) 10
      liftIO $ sanitizeLHEFile pids unweightedevtfilename rawunweightedevtfilename
  return () 

-- | run PYTHIA as a user-defined process.
runPYTHIA :: (Model a) => WorkIO a () 
runPYTHIA = do
  WS _ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  let bindir = wdir </> "bin"
      eventdir = wdir </> "Events" 
      carddir  = wdir </> "Cards"
      pythiadir = wdir </> "../pythia-pgs/src"
      taskname = makeRunName psetup rsetup 
      unweightedevtfilename = taskname ++ "_unweighted_events.lhe" 
      rawunweightedevtfilename = "unweighted_events.lhe"
      -- hepfilename = taskname++"_pythia_events.hep"
  liftIO $ setCurrentDirectory eventdir
  liftIO $ renameFile (carddir </> "pythia_card.dat.sanitize") (carddir </> "pythia_card.dat")
  checkFile (eventdir </> rawunweightedevtfilename) 10
  b <- liftIO $ doesFileExist rawunweightedevtfilename
  if b 
    then do 
      debugMsgDef "Start PYTHIA"
      liftIO $ do putEnv  $ "PDG_MASS_TBL=" ++ pythiadir </> "mass_width_2004.mc "
                  readProcessWithExitCode (bindir </> "run_pythia") [] ""
      liftIO $ threadDelay ( 60 * 1000000 )
      checkFile (eventdir </> "pythia_events.hep") 10
--      liftIO $ renameFile "pythia_events.hep" hepfilename
      liftIO $ copyFile rawunweightedevtfilename unweightedevtfilename
      liftIO $ system $ "gzip -f " ++ unweightedevtfilename
      checkFile (unweightedevtfilename <.> "gz") 10
    else throwError "ERROR: No unweighted events" 
  return ()

-- | 
runHEP2LHE :: (Model a) => WorkIO a () 
runHEP2LHE = do
  WS ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  let eventdir = wdir </> "Events" 
      pythiadir = wdir </> "../pythia-pgs/src"
      taskname = makeRunName psetup rsetup 
      hepfilename = case lhesanitizer rsetup of 
                      NoLHESanitize -> taskname++"_pythia_events.hep"
                      LHESanitize _ -> "pythia_events.hep"
  let hep2lhe = case usercut rsetup of 
                  UserCutDef _ -> (sandboxdir ssetup) </> "hep2lhe.iw"
                  NoUserCutDef -> pythiadir </> "hep2lhe"
      hep2lhe_result = case usercut rsetup of 
                         UserCutDef _ -> "afterusercut.hepevt" 
                         NoUserCutDef -> "pythia_events.lhe"
  liftIO $ setCurrentDirectory eventdir
  checkFile (eventdir </> hepfilename) 10 
  b <- liftIO $ doesFileExist hepfilename 
  if b 
    then do 
      debugMsgDef "Start hep2lhe"
      workIOReadProcessWithExitCode  hep2lhe [hepfilename,hep2lhe_result] "" 
      case usercut rsetup of
        UserCutDef _  -> return () 
        NoUserCutDef  -> do 
          liftIO $ renameFile "pythia_events.lhe"  (taskname ++ "_pythia_events.lhe")
          liftIO $ system $ "gzip -f " ++ (taskname ++ "_pythia_events.lhe")
          return () 
    else throwError "ERROR pythia result does not exist"  
  return () 

-- | 
runHEPEVT2STDHEP :: (Model a) => WorkIO a () 
runHEPEVT2STDHEP = do
  WS ssetup _psetup _ _ _ <- ask 
  wdir <- getWorkDir
  let eventdir = wdir </> "Events" 
      hepevtfilename = "afterusercut.hepevt"  
      stdhepfilename = "afterusercut.stdhep" 
  liftIO $ setCurrentDirectory eventdir
  checkFile (eventdir </> hepevtfilename) 10 
  b <- liftIO $ doesFileExist hepevtfilename 
  if b 
    then do 
      debugMsgDef "Start hepevt2stdhep"
      workIOReadProcessWithExitCode (sandboxdir ssetup </> "hepevt2stdhep.iw") 
                                       [hepevtfilename,stdhepfilename] "" 
    else throwError "ERROR pythia result does not exist"  
  return () 

-- | 
runPGS :: (Model a) => WorkIO a () 
runPGS = do
  WS _ssetup _psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  let eventdir = wdir </> "Events" 
      pgsdir   = wdir </> "../pythia-pgs/src"
      carddir  = wdir </> "Cards"
      stdhepfilename = "afterusercut.stdhep" 
      hepfilename = "pythia_events.hep"
      uncleanedfilename = "pgs_uncleaned.lhco"
  liftIO $ setCurrentDirectory eventdir
  checkFile (carddir </> "pgs_card.dat.user") 10 
  liftIO $ renameFile (carddir </> "pgs_card.dat.user") (carddir </> "pgs_card.dat")
  let pythiaresult = case usercut rsetup of
                       UserCutDef _ -> stdhepfilename
                       NoUserCutDef -> hepfilename 
  checkFile (eventdir </> pythiaresult) 10
  b <- liftIO $ doesFileExist pythiaresult 
  if b 
    then do 
      debugMsgDef "Start pgs"
      liftIO $ do 
        putEnv  $ "PDG_MASS_TBL=" ++ pgsdir </> "mass_width_2004.mc "
        readProcessWithExitCode (pgsdir </> "pgs") ["--stdhep",pythiaresult,"--nev","0","--detector","../Cards/pgs_card.dat",uncleanedfilename] "" 
    else throwError "ERROR pythia result does not exist"  
  return () 

-- | 
runClean :: (Model a) => WorkIO a () 
runClean = do
  WS _ssetup psetup rsetup _ _ <- ask
  wdir <- getWorkDir 
  let eventdir = wdir </> "Events" 
      pgsdir   = wdir </> "../pythia-pgs/src"
      taskname = makeRunName psetup rsetup 
      -- hepfilename = taskname++"_pythia_events.hep"
      -- hepevtfilename = "afterusercut.hepevt"  
      -- stdhepfilename = "afterusercut.stdhep" 
      uncleanedfilename = "pgs_uncleaned.lhco"
      cleanedfilename = "pgs_cleaned.lhco"
      finallhco = taskname ++ "_pgs_events.lhco"
      -- finallhcogz = taskname ++ "_pgs_events.lhco.gz"
  liftIO $ setCurrentDirectory eventdir
  checkFile (eventdir </> uncleanedfilename) 10
  b <- liftIO $ doesFileExist uncleanedfilename 
  if b 
    then do 
      debugMsgDef "Start clean_output"
      workIOReadProcessWithExitCode (pgsdir </> "clean_output") [ "-muon", uncleanedfilename, cleanedfilename ] "" 
      liftIO $ renameFile (eventdir </> cleanedfilename) (eventdir </> finallhco)
      liftIO $ system ("gzip -f " ++ finallhco) 
    else throwError "ERROR pythia result does not exist"  
  return () 

-- | 
updateBanner :: (Model a) => WorkIO a () 
updateBanner = do
  WS _ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  case (usercut rsetup) of 
    NoUserCutDef -> return () 
    UserCutDef uc -> do  
      let eventdir = wdir </> "Events" 
          taskname = makeRunName psetup rsetup 
          carddir  = wdir </> "Cards"
          bannerfilename = taskname ++ "_banner.txt"
          newbannerfilename = taskname ++ "_newbanner.txt"
          usercutcontent = prettyprintUserCut uc
      liftIO $ setCurrentDirectory eventdir
      checkFile (eventdir </> bannerfilename) 10
      bannerstr  <- liftIO $ readFile (eventdir </> bannerfilename)
      pgscardstr <- liftIO $ readFile (carddir </> "pgs_card.dat")  
      let newbannerstr = bannerstr ++ usercutcontent ++ pgscardstr
      liftIO $ writeFile (eventdir </> newbannerfilename) newbannerstr 

-- |
cleanHepFiles :: (Model a) => WorkIO a () 
cleanHepFiles = do 
  WS _ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  let taskname = makeRunName psetup rsetup 
      eventdir = wdir </> "Events" 
      existThenRemoveForAny x = existThenRemove (eventdir </> x)
      clean = mapM_ existThenRemoveForAny  
      hepfilename = taskname++"_pythia_events.hep"
      hepevtfilename = "afterusercut.hepevt"  
      stdhepfilename = "afterusercut.stdhep"      
      uncleanedfilename = "pgs_uncleaned.lhco"
      cleanedfilename = "pgs_cleaned.lhco"
      onlyhep = [ hepfilename ] 
      allhep  = [ hepfilename
                , hepevtfilename
                , stdhepfilename
                , uncleanedfilename
                , cleanedfilename ]
      dellst = case (pythia rsetup, match rsetup, usercut rsetup) of 
                 (NoPYTHIA,NoMatch,_) -> []
                 (_,MLM,NoUserCutDef) -> onlyhep
                 (_,MLM,UserCutDef _) -> allhep
                 (RunPYTHIA,_,NoUserCutDef) -> onlyhep
                 (RunPYTHIA,_,UserCutDef _) -> allhep
  liftIO $ sleep 5
  clean dellst

-- | 
cleanAll :: (Model a) => WorkIO a () 
cleanAll = do 
  WS _ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  let taskname = makeRunName psetup rsetup 
      eventdir = wdir </> "Events" 
      existThenRemoveForAny x = existThenRemove (eventdir </> x)
      clean = mapM_ existThenRemoveForAny  
      hepfilename = taskname++"_pythia_events.hep"
      hepevtfilename = "afterusercut.hepevt"  
      stdhepfilename = "afterusercut.stdhep"      
      uncleanedfilename = "pgs_uncleaned.lhco"
      cleanedfilename = "pgs_cleaned.lhco"
      bannerfile    = taskname++ "_banner.txt"
      treefile1     = taskname++ "_beforeveto.tree.gz"
      lheeventfile1 = taskname ++ "_events.lhe.gz"
      treefile2     = taskname ++ "_events.tree.gz"
      newbannerfile = taskname ++ "_newbanner.txt"
      pgseventfile  = taskname ++ "_pgs_events.lhco.gz"
      plotpythiafile = taskname ++ "_plots_pythia.html"
      pythiadir      = taskname ++ "_pythia"
      pythialog      = taskname ++ "_pythia.log"
      pythiaroot     = taskname ++ "_pythia.root"
      pythialhe      = taskname ++ "_pythia_events.lhe.gz"
      unweightedevts = taskname ++ "_unweighted_events.lhe.gz"
      xsecstree      = taskname ++ "_xsecs.tree"
      allfiles  = [ hepfilename
                  , hepevtfilename
                  , stdhepfilename
                  , uncleanedfilename
                  , cleanedfilename 
                  , bannerfile
                  , treefile1
                  , lheeventfile1
                  , treefile2
                  , newbannerfile
                  , pgseventfile
                  , plotpythiafile
                  , pythiadir
                  , pythialog
                  , pythiaroot
                  , pythialhe
                  , unweightedevts
                  , xsecstree ]
  liftIO $ sleep 5
  clean allfiles
  b <- liftIO $ doesDirectoryExist ( eventdir </> pythiadir)
  if b 
    then do
      liftIO $ setCurrentDirectory ( eventdir </> pythiadir ) 
      liftIO $ system "rm *"
      liftIO $ setCurrentDirectory eventdir 
      liftIO $ removeDirectory (eventdir </> pythiadir )
    else return () 

-- |      
makeHepGz :: (Model a) => WorkIO a () 
makeHepGz = do 
  WS _ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  let taskname = makeRunName psetup rsetup 
      eventdir = wdir </> "Events" 
      hepfilename = taskname++"_pythia_events.hep"
  liftIO $ setCurrentDirectory eventdir
  b <- liftIO $ doesFileExist (hepfilename <.> "gz")
  if b 
    then return () 
    else case (pythia rsetup, match rsetup, usercut rsetup, uploadhep rsetup) of 
           (_,MLM,_,UploadHEP) -> do 
              checkFile hepfilename 10 
              liftIO $ system $ "gzip -f " ++ hepfilename 
              return ()
           (RunPYTHIA,_,_,UploadHEP) -> do 
              checkFile hepfilename 10 
              liftIO $ system $ "gzip -f " ++ hepfilename
              return () 
           _ -> return () 
  liftIO $ threadDelay 5000000
  return ()

