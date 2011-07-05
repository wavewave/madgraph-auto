{-# LANGUAGE ScopedTypeVariables, PackageImports #-}

module HEP.Automation.MadGraph.Run where

import System.Process
import System.Directory
import System.Posix.Unistd (sleep)
import System.Posix.Files
import System.Posix.Env 

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Error

import System.Exit
import System.FilePath ((</>))

import HEP.Automation.MadGraph.Util
import HEP.Automation.MadGraph.Model 
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.SetupType

import Text.StringTemplate
import Text.StringTemplate.Helpers

workIOReadProcessWithExitCode :: FilePath -> [String] -> String -> WorkIO a ()
workIOReadProcessWithExitCode cmd args input = do 
  (ex, out, _err) <- liftIO $ readProcessWithExitCode cmd args input
  case ex of 
    ExitSuccess -> return () 
    ExitFailure c -> throwError $ "error exit code = " ++ show c ++ " while running " ++ cmd ++ " " ++ show args 
                                  ++ "\n stdout = " ++ out 

checkFile :: (Model a) => FilePath -> Int -> WorkIO a () 
checkFile fp n = do 
  if n < 0 
    then throwError $ "no " ++ fp ++ " ever created." 
    else do 
      b <- liftIO $ doesFileExist fp 
      if b  
        then do 
          b2 <- liftIO $ getFileStatus fp >>= return.(>0).fromIntegral.fileSize
          if b2 then liftIO $ do { putStrLn $ fp ++ " checked" ; return () } 
                else do { liftIO (threadDelay 5000000); checkFile fp (n-1) } 
          else do { liftIO $ threadDelay 5000000 ; checkFile fp (n-1) }  

checkVetoFile :: (Model a) => FilePath -> Int -> WorkIO a () 
checkVetoFile fp n = do 
  if n < 0 
    then throwError $ fp ++ " still exists "
    else do 
      b <- liftIO $ doesFileExist fp 
      if not b 
        then liftIO $ do { putStrLn $ fp ++ " is not exist. good"; return ()}
        else do { liftIO $ threadDelay 5000000; checkVetoFile fp (n-1) }

existThenRemove :: (Model a) => FilePath -> WorkIO a () 
existThenRemove fp = do 
  b <- liftIO $ doesFileExist fp 
  if b 
    then do { liftIO $ removeFile fp; checkVetoFile fp 3 }  
    else return () 

checkDirectory :: (Model a) => FilePath -> Int -> WorkIO a () 
checkDirectory fp n = do 
  if n < 0 
    then throwError $ "no " ++ fp ++ " ever created." 
    else do 
      b <- liftIO $ doesDirectoryExist fp 
      if b  
         then liftIO $ do { putStrLn $ fp ++ " checked" ; return () } 
         else do { liftIO $ threadDelay 5000000 ; checkDirectory fp (n-1) }  


compileshSetup :: ScriptSetup -> IO String 
compileshSetup ssetup = do
  let mg4base = mg5base ssetup </> ".."
  templates <- directoryGroup (templatedir ssetup)      
  return $ (renderTemplateGroup 
              templates 
              [ ("mgfourbase", mg4base) ]
              "compile.sh") ++ "\n\n\n"


compileFortran :: (Model a) => WorkIO a ()
compileFortran = do 
  WS ssetup _ rsetup _ _ <- ask   
  case (usercut rsetup) of 
    NoUserCutDef -> return () 
    UserCutDef uc -> do 
      liftIO $ putStrLn $ "set up fortran program" 
      let existThenRemoveForAny x = existThenRemove (workingdir ssetup </> x)
          cpFrmTmpl2Working x = copyFile (templatedir ssetup </> x) 
                                         (workingdir ssetup </> x)
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
      hep2lhe <- liftIO $ hep2lheSetup (templatedir ssetup) uc
      liftIO $ writeFile (workingdir ssetup </> "hep2lhe.f") hep2lhe

      -- setup new compile.sh according to system configuration.
      compilesh  <- liftIO $ compileshSetup ssetup 
      liftIO $ writeFile (workingdir ssetup </> "compile.sh") compilesh

      -- copy files and compile
      liftIO $ mapM_ cpFrmTmpl2Working filelistNoTemplate 
      liftIO $ setCurrentDirectory (workingdir ssetup)
      checkFile (workingdir ssetup </> "compile.sh") 10 
      liftIO $ threadDelay 1000000
      workIOReadProcessWithExitCode "sh" ["./compile.sh"] "" 

      return ()



createWorkDir :: (Model a) => ScriptSetup -> ProcessSetup a -> WorkIO a ()
createWorkDir ssetup psetup = do 
  liftIO $ putStrLn $ "set up a working directory" 
  let processfilecontent = makeProcessFile (model psetup) (mversion psetup) (process psetup) (workname psetup)
  liftIO $ writeFile (workingdir ssetup </> "proc_card_mg5.dat") processfilecontent
  checkFile (workingdir ssetup </> "proc_card_mg5.dat") 10 
  liftIO $ setCurrentDirectory (mg5base ssetup)
  workIOReadProcessWithExitCode ("bin/mg5") [workingdir ssetup </> "proc_card_mg5.dat"] ""
  checkDirectory (mg5base ssetup </> workname psetup) 10
  checkDirectory (mg5base ssetup </> workname psetup </> "SubProcesses") 10
  liftIO $ putStrLn $ "moving directory" 
                      ++ (mg5base ssetup </> workname psetup) 
                      ++ " to " 
                      ++ (workbase ssetup </> workname psetup) 
  liftIO $ renameDirectory (mg5base ssetup </> workname psetup) (workbase ssetup </> workname psetup) 
  return () 

{-
replicateWorkDir :: (Model a) => String -> ScriptSetup -> ClusterSetup a -> IO () 
replicateWorkDir masterworkname ssetup csetup = do 
  let slaveworkname = cluster_workname . cluster $ csetup 
  putStrLn $ "make copies of " ++ masterworkname ++ " to " ++ slaveworkname 
  setCurrentDirectory (workbase ssetup) 
  readProcessWithExitCode ("cp") ["-a", masterworkname, slaveworkname ] "" 
  return ()  -}
  
getWorkDir :: (Model a) => WorkIO a FilePath   
getWorkDir = do 
  WS ssetup psetup _rsetup csetup _ <- ask   
  case cluster csetup of 
    Cluster _ cluname -> return $ workbase ssetup </> cluname 
    _                 -> return $ workbase ssetup </> workname psetup


cardPrepare :: (Model a) => WorkIO a () 
cardPrepare = do 
  WS ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  let taskname = makeRunName psetup rsetup 
  let carddir = wdir </> "Cards"
  liftIO $ putStrLn $ "prepare for cards for " ++ taskname
  checkDirectory wdir 10
  checkDirectory (wdir </> "SubProcesses") 10
  checkDirectory carddir 10   

  -- erase previous run 
  existThenRemove (carddir </> "param_card.dat") 
  existThenRemove (carddir </> "run_card.dat") 
  existThenRemove (carddir </> "pythia_card.dat") 
  existThenRemove (carddir </> "pgs_card.dat")
  
  paramcard  <- liftIO $ paramCardSetup 
                           (templatedir ssetup)
                           (model psetup)
                           (param rsetup)
  
  runcard    <- liftIO $ runCardSetup 
                           (templatedir ssetup)
                           (machine rsetup) 
                           (cut     rsetup) 
                           (match   rsetup) 
                           (rgrun   rsetup) 
                           (rgscale rsetup) 
		           (numevent rsetup) 
                  
  pythiacard <- liftIO $ pythiaCardSetup 
                           (templatedir ssetup)
                           (match   rsetup)
                           (pythia  rsetup) 
                  
  pgscard    <- liftIO $ pgsCardSetup
                           (templatedir ssetup)
                           (machine rsetup)
                           (pgs     rsetup) 
                           (jetalgo rsetup)
                  
  liftIO $ writeFile (carddir </> "param_card.dat") paramcard
  liftIO $ writeFile (carddir </> "run_card.dat")   runcard

  case pythiacard of 
    Nothing  -> return () 
    Just str -> liftIO $ writeFile (carddir </> "pythia_card.dat") str
    
  case pgscard  of 
    Nothing  -> return () 
    Just str -> case usercut rsetup of 
      NoUserCutDef  -> liftIO $ writeFile (carddir </> "pgs_card.dat") str
      UserCutDef _  -> liftIO $ writeFile (carddir </> "pgs_card.dat.user") str

  case pgs rsetup of 
    RunPGSNoTau -> do 
      liftIO $ copyFile (templatedir ssetup </> "run_pgs_notau" ) (workbase ssetup </> workname psetup </> "bin" </> "run_pgs" )
      liftIO $ setFileMode (workbase ssetup </> workname psetup </> "bin" </> "run_pgs") 0o755 
        
    _ -> return () 
  return () 

generateEvents :: (Model a) => WorkIO a () 
generateEvents = do 
  WS _ssetup psetup rsetup csetup _ <- ask
  wdir <- getWorkDir
  let taskname = makeRunName psetup rsetup 
  liftIO $ putStrLn $ "generating event for " ++ taskname
  liftIO $ setCurrentDirectory wdir 
  checkFile (wdir </> "Cards/run_card.dat") 10
  checkFile (wdir </> "Cards/param_card.dat") 10

  case pythia rsetup of 
    RunPYTHIA -> checkFile (wdir </> "Cards/pythia_card.dat") 10
    NoPYTHIA -> return () 

  case (pgs rsetup, usercut rsetup)  of 
    (RunPGS,NoUserCutDef) -> checkFile (wdir </> "Cards/pgs_card.dat") 10
    (RunPGSNoTau,NoUserCutDef) -> checkFile (wdir </> "Cards/pgs_card.dat") 10      
    (RunPGS,UserCutDef _) -> checkFile (wdir </> "Cards/pgs_card.dat.user") 10
    (RunPGSNoTau,UserCutDef _) -> checkFile (wdir </> "Cards/pgs_card.dat.user") 10
    (NoPGS,_)  -> return () 

  case cluster csetup of
    NoParallel     -> workIOReadProcessWithExitCode ("bin/generate_events") ["0", taskname] ""
    Parallel ncore -> workIOReadProcessWithExitCode ("bin/generate_events") ["2", show ncore, taskname] ""
--      Cluster cname -> readProcess ("bin/generate_events") ["1", cname, taskname] "" 
    Cluster _ _ -> undefined 
  return ()

runHEP2LHE :: (Model a) => WorkIO a () 
runHEP2LHE = do
  WS ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  let eventdir = wdir </> "Events" 
      taskname = makeRunName psetup rsetup 
      hepfilename = taskname++"_pythia_events.hep"
      hepevtfilename = "afterusercut.hepevt"  

  liftIO $ setCurrentDirectory eventdir
  checkFile (eventdir </> hepfilename) 10 
  b <- liftIO $ doesFileExist hepfilename 
  if b 
    then do 
      liftIO $ putStrLn "Start hep2lhe"
      workIOReadProcessWithExitCode  (workingdir ssetup </> "hep2lhe.iw") 
                                        [hepfilename,hepevtfilename] "" 
    else throwError "ERROR pythia result does not exist"  
  return () 

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
      liftIO $ putStrLn "Start hepevt2stdhep"
      workIOReadProcessWithExitCode (workingdir ssetup </> "hepevt2stdhep.iw") 
                                       [hepevtfilename,stdhepfilename] "" 
    else throwError "ERROR pythia result does not exist"  
  return () 

runPGS :: (Model a) => WorkIO a () 
runPGS = do
  WS _ssetup _psetup _ _ _ <- ask 
  wdir <- getWorkDir 
  let eventdir = wdir </> "Events" 
      pgsdir   = wdir </> "../pythia-pgs/src"
      carddir  = wdir </> "Cards"
      stdhepfilename = "afterusercut.stdhep" 
      uncleanedfilename = "pgs_uncleaned.lhco"
  liftIO $ setCurrentDirectory eventdir
  checkFile (carddir </> "pgs_card.dat.user") 10 
  liftIO $ renameFile (carddir </> "pgs_card.dat.user") (carddir </> "pgs_card.dat")
  checkFile (eventdir </> stdhepfilename) 10
  b <- liftIO $ doesFileExist stdhepfilename 
  if b 
    then liftIO $ do putStrLn "Start pgs"
                     putEnv  $ "PDG_MASS_TBL=" ++ pgsdir </> "mass_width_2004.mc "
                     readProcessWithExitCode (pgsdir </> "pgs") ["--stdhep",stdhepfilename,"--nev","0","--detector","../Cards/pgs_card.dat",uncleanedfilename] "" 
    else throwError "ERROR pythia result does not exist"  
  return () 

runClean :: (Model a) => WorkIO a () 
runClean = do
  WS _ssetup psetup rsetup _ _ <- ask
  wdir <- getWorkDir 
  let eventdir = wdir </> "Events" 
      pgsdir   = wdir </> "../pythia-pgs/src"
      taskname = makeRunName psetup rsetup 
      -- hepfilename = taskname++"_pythia_events.hep"
      -- hepevtfilename = "afterusercut.hepevt"  
      stdhepfilename = "afterusercut.stdhep" 
      uncleanedfilename = "pgs_uncleaned.lhco"
      cleanedfilename = "pgs_cleaned.lhco"
      finallhco = taskname ++ "_pgs_events.lhco"
      -- finallhcogz = taskname ++ "_pgs_events.lhco.gz"
  liftIO $ setCurrentDirectory eventdir
  checkFile (eventdir </> stdhepfilename) 10
  b <- liftIO $ doesFileExist stdhepfilename 
  if b 
    then do 
      liftIO $ putStrLn "Start clean_output"
      workIOReadProcessWithExitCode (pgsdir </> "clean_output") [ "-muon", uncleanedfilename, cleanedfilename ] "" 
      liftIO $ renameFile (eventdir </> cleanedfilename) (eventdir </> finallhco)
      liftIO $ system ("gzip -f " ++ finallhco) 
    else throwError "ERROR pythia result does not exist"  
  return () 

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

makeHepGz :: (Model a) => WorkIO a () 
makeHepGz = do 
  WS _ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  let taskname = makeRunName psetup rsetup 
      eventdir = wdir </> "Events" 
      hepfilename = taskname++"_pythia_events.hep"
  liftIO $ setCurrentDirectory eventdir
  case (pythia rsetup, match rsetup, usercut rsetup, uploadhep rsetup) of 
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
