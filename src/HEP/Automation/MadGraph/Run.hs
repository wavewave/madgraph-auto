{-# LANGUAGE ScopedTypeVariables, PackageImports #-}

module HEP.Automation.MadGraph.Run where

import System.Process
import System.Directory
import System.Posix.Unistd (sleep)
import System.Posix.Env 

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader

import System.FilePath ((</>))

import HEP.Automation.MadGraph.Util
import HEP.Automation.MadGraph.Model 
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.SetupType

import Text.StringTemplate
import Text.StringTemplate.Helpers


checkFile :: FilePath -> Int -> IO () 
checkFile fp n = do 
  if n < 0 
    then error $ "no " ++ fp ++ " ever created." 
    else do 
      b <- doesFileExist fp 
      if b  
         then do { putStrLn $ fp ++ " checked" ; return () } 
         else do { threadDelay 5000000 ; checkFile fp (n-1) }  

checkDirectory :: FilePath -> Int -> IO () 
checkDirectory fp n = do 
  if n < 0 
    then error $ "no " ++ fp ++ " ever created." 
    else do 
      b <- doesDirectoryExist fp 
      if b  
         then do { putStrLn $ fp ++ " checked" ; return () } 
         else do { threadDelay 5000000 ; checkDirectory fp (n-1) }  


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
  liftIO $ 
    case (usercut rsetup) of 
      NoUserCutDef -> return () 
      UserCutDef uc -> do 
        putStrLn $ "set up fortran program" 
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
                                    , "ME2pythia.f" ]
        -- erase previous run 
        mapM_ existThenRemoveForAny  (("hep2lhe.f") : filelistNoTemplate)

        -- setup new hep2lhe.f with a given user cut 
        hep2lhe    <- hep2lheSetup (templatedir ssetup) uc
        writeFile (workingdir ssetup </> "hep2lhe.f") hep2lhe

       -- setup new compile.sh according to system configuration.
        compilesh  <- compileshSetup ssetup 
        writeFile (workingdir ssetup </> "compile.sh") compilesh

        -- copy files and compile
        mapM_ cpFrmTmpl2Working filelistNoTemplate 
        setCurrentDirectory (workingdir ssetup)
        readProcess "sh" ["./compile.sh"] "" 

        return ()


createWorkDir :: (Model a) => ScriptSetup -> ProcessSetup a -> IO ()
createWorkDir ssetup psetup = do 
  putStrLn $ "set up a working directory" 
  let processfilecontent = makeProcessFile (model psetup) (mversion psetup) (process psetup) (workname psetup)
  writeFile (workingdir ssetup </> "proc_card_mg5.dat") processfilecontent
  checkFile (workingdir ssetup </> "proc_card_mg5.dat") 10 
  setCurrentDirectory (mg5base ssetup)
  readProcess ("bin/mg5") [workingdir ssetup </> "proc_card_mg5.dat"] ""
  checkDirectory (mg5base ssetup </> workname psetup) 10
  putStrLn $ "moving directory" 
             ++ (mg5base ssetup </> workname psetup) 
             ++ " to " 
             ++ (workbase ssetup </> workname psetup) 
  renameDirectory (mg5base ssetup </> workname psetup) (workbase ssetup </> workname psetup) 
  
  return () 

replicateWorkDir :: (Model a) => String -> ScriptSetup -> ClusterSetup a -> IO () 
replicateWorkDir masterworkname ssetup csetup = do 
  let slaveworkname = cluster_workname . cluster $ csetup 
  putStrLn $ "make copies of " ++ masterworkname ++ " to " ++ slaveworkname 
  setCurrentDirectory (workbase ssetup) 
  readProcess ("cp") ["-a", masterworkname, slaveworkname ] "" 
  return () 
  
getWorkDir :: (Model a) => WorkIO a FilePath   
getWorkDir = do 
  WS ssetup psetup rsetup csetup _ <- ask   
  case cluster csetup of 
    Cluster master cluname -> return $ workbase ssetup </> cluname 
    _                      -> return $ workbase ssetup </> workname psetup


cardPrepare :: (Model a) => WorkIO a () 
cardPrepare = do 
  WS ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  liftIO $ do 
    let taskname = makeRunName psetup rsetup 
    putStrLn $ "prepare for cards for " ++ taskname
  
  
    let carddir = wdir </> "Cards"
    checkDirectory carddir 10   
    -- erase previous run 
    existThenRemove (carddir </> "param_card.dat") 
    existThenRemove (carddir </> "run_card.dat") 
    existThenRemove (carddir </> "pythia_card.dat") 
    existThenRemove (carddir </> "pgs_card.dat")
  
    paramcard  <- paramCardSetup 
                    (templatedir ssetup)
                    (model psetup)
                    (param rsetup)
  
    runcard    <- runCardSetup 
                    (templatedir ssetup)
                    (machine rsetup) 
                    (cut     rsetup) 
                    (match   rsetup) 
                    (rgrun   rsetup) 
                    (rgscale rsetup) 
		    (numevent rsetup) 
                  
    pythiacard <- pythiaCardSetup 
                    (templatedir ssetup)
                    (match   rsetup)
                    (pythia  rsetup) 
                  
    pgscard    <- pgsCardSetup
                    (templatedir ssetup)
                    (machine rsetup)
                    (pgs     rsetup) 
                  
    writeFile (carddir </> "param_card.dat") paramcard
    writeFile (carddir </> "run_card.dat")   runcard

    case pythiacard of 
      Nothing -> return () 
      Just str -> writeFile (carddir </> "pythia_card.dat") str
    
    case pgscard  of 
      Nothing -> return () 
      Just str -> case usercut rsetup of 
        NoUserCutDef  -> writeFile (carddir </> "pgs_card.dat") str
        UserCutDef _  -> writeFile (carddir </> "pgs_card.dat.user") str

    case pgs rsetup of 
      RunPGSNoTau -> copyFile (workingdir ssetup </> "run_pgs_notau" ) (workbase ssetup </> workname psetup </> "bin" </> "run_pgs" )
      _ -> return () 
    return () 



generateEvents :: (Model a) => WorkIO a () 
generateEvents = do 
  WS ssetup psetup rsetup csetup _ <- ask
  wdir <- getWorkDir
  liftIO $ do 
    let taskname = makeRunName psetup rsetup 
  
    putStrLn $ "generating event for " ++ taskname
    setCurrentDirectory wdir 
    checkFile (wdir </> "Cards/run_card.dat") 10
    checkFile (wdir </> "Cards/param_card.dat") 10

    case pythia rsetup of 
      RunPYTHIA -> checkFile (wdir </> "Cards/pythia_card.dat") 10
      NoPYTHIA -> return () 

    case pgs rsetup of 
      RunPGS -> checkFile (wdir </> "Cards/pgs_card.dat") 10
      NoPGS  -> return () 


    
    case cluster csetup of
      NoParallel -> readProcess ("bin/generate_events") ["0", taskname] ""
      Parallel ncore -> readProcess ("bin/generate_events") ["2", show ncore, taskname] ""
--      Cluster cname -> readProcess ("bin/generate_events") ["1", cname, taskname] "" 
      Cluster _ _ -> undefined 
    return ()

runHEP2LHE :: (Model a) => WorkIO a () 
runHEP2LHE = do
  WS ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  liftIO $ do
    let eventdir = wdir </> "Events" 
        taskname = makeRunName psetup rsetup 
        hepfilename = taskname++"_pythia_events.hep"
        hepevtfilename = "afterusercut.hepevt"  
    setCurrentDirectory eventdir
    checkFile (eventdir </> hepfilename) 10 
    b <- doesFileExist hepfilename 
    if b 
      then do 
        putStrLn "Start hep2lhe"
        readProcess (workingdir ssetup </> "hep2lhe.iw") 
                    [hepfilename,hepevtfilename] "" 
      else error "ERROR pythia result does not exist"  
    return () 

runHEPEVT2STDHEP :: (Model a) => WorkIO a () 
runHEPEVT2STDHEP = do
  WS ssetup psetup _ _ _ <- ask 
  wdir <- getWorkDir 
  liftIO $ do 
    let eventdir = wdir </> "Events" 
        hepevtfilename = "afterusercut.hepevt"  
        stdhepfilename = "afterusercut.stdhep" 
  
    setCurrentDirectory eventdir
    checkFile (eventdir </> hepevtfilename) 10 
    b <- doesFileExist hepevtfilename 
    if b 
      then do 
        putStrLn "Start hepevt2stdhep"
        readProcess (workingdir ssetup </> "hepevt2stdhep.iw") 
                    [hepevtfilename,stdhepfilename] "" 
      else error "ERROR pythia result does not exist"  
    return () 

runPGS :: (Model a) => WorkIO a () 
runPGS = do
  WS ssetup psetup _ _ _ <- ask 
  wdir <- getWorkDir 
  liftIO $ do
    let eventdir = wdir </> "Events" 
        pgsdir   = wdir </> "../pythia-pgs/src"
        carddir  = wdir </> "Cards"
        stdhepfilename = "afterusercut.stdhep" 
        uncleanedfilename = "pgs_uncleaned.lhco"
    setCurrentDirectory eventdir
    checkFile (carddir </> "pgs_card.dat.user") 10 
    renameFile (carddir </> "pgs_card.dat.user") (carddir </> "pgs_card.dat")
    checkFile (eventdir </> stdhepfilename) 10
    b <- doesFileExist stdhepfilename 
    if b 
      then do 
        putStrLn "Start pgs"
        putEnv  $ "PDG_MASS_TBL=" ++ pgsdir </> "mass_width_2004.mc "
        readProcess (pgsdir </> "pgs") ["--stdhep",stdhepfilename,"--nev","0","--detector","../Cards/pgs_card.dat",uncleanedfilename] "" 
      else error "ERROR pythia result does not exist"  
    return () 

runClean :: (Model a) => WorkIO a () 
runClean = do
  WS ssetup psetup rsetup _ _ <- ask
  wdir <- getWorkDir 
  liftIO $ do
    let eventdir = wdir </> "Events" 
        pgsdir   = wdir </> "../pythia-pgs/src"
        taskname = makeRunName psetup rsetup 
        -- hepfilename = taskname++"_pythia_events.hep"
        -- hepevtfilename = "afterusercut.hepevt"  
        stdhepfilename = "afterusercut.stdhep" 
        uncleanedfilename = "pgs_uncleaned.lhco"
        cleanedfilename = "pgs_cleaned.lhco"
        finallhco = taskname ++ "_pgs_events.lhco"

    setCurrentDirectory eventdir
    checkFile (eventdir </> stdhepfilename) 10
    b <- doesFileExist stdhepfilename 
    if b 
      then do 
        putStrLn "Start clean_output"
        readProcess (pgsdir </> "clean_output") [ "-muon", uncleanedfilename, cleanedfilename ] "" 
        renameFile (eventdir </> cleanedfilename) (eventdir </> finallhco)
      else error "ERROR pythia result does not exist"  
    return () 

updateBanner :: (Model a) => WorkIO a () 
updateBanner = do
  WS ssetup psetup rsetup _ _ <- ask 
  wdir <- getWorkDir 
  liftIO $ do 
    case (usercut rsetup) of 
      NoUserCutDef -> return () 
      UserCutDef uc -> do  
        let eventdir = wdir </> "Events" 
            taskname = makeRunName psetup rsetup 
            carddir  = wdir </> "Cards"
            bannerfilename = taskname ++ "_banner.txt"
            newbannerfilename = taskname ++ "_newbanner.txt"
            usercutcontent = prettyprintUserCut uc
        setCurrentDirectory eventdir
        checkFile (eventdir </> bannerfilename) 10
        bannerstr  <- readFile (eventdir </> bannerfilename)
        pgscardstr <- readFile (carddir </> "pgs_card.dat")  
        let newbannerstr = bannerstr ++ usercutcontent ++ pgscardstr
        writeFile (eventdir </> newbannerfilename) newbannerstr 

cleanHepFiles :: (Model a) => WorkIO a () 
cleanHepFiles = do 
  WS ssetup psetup rsetup _ _ <- ask 
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
  liftIO $ do 
    sleep 5
    clean dellst
