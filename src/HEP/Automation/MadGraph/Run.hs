{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.Run 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- main run script for madgraph/pythia/pgs  
--
-----------------------------------------------------------------------------

module HEP.Automation.MadGraph.Run where

-- from others 
import Control.Applicative ((<$>),(<*>))
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
import HEP.Parser.LHE.Sanitizer
import Text.StringTemplate.Helpers
-- from this package
import HEP.Automation.MadGraph.Card
import HEP.Automation.MadGraph.Log
import HEP.Automation.MadGraph.Model 
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
import HEP.Automation.MadGraph.Util

(#) :: a -> (a -> b) -> b
x # f = f x

infixr 9 #

-- | 
compileshSetup :: ScriptSetup -> IO String 
compileshSetup ssetup = do
  let mg4base = mg5base ssetup </> ".."
  templates <- directoryGroup (runtmpldir ssetup)      
  return $ (renderTemplateGroup 
              templates 
              [ ("mgfourbase", mg4base) ]
              "compile.sh") ++ "\n\n\n"



-- | Creating working directory. 
--   Working directory is an autonomous directory of a single madgraph setup
createWorkDir :: (Model a) => ScriptSetup -> ProcessSetup a -> WorkIO a ()
createWorkDir ssetup psetup = do 
  debugMsgDef "set up a working directory" 
  let processfilecontent = makeProcessFile (model psetup) (process psetup) (workname psetup)
  liftIO $ writeFile (sandboxdir ssetup </> "proc_card_mg5.dat") processfilecontent
  -- checkFile (sandboxdir ssetup </> "proc_card_mg5.dat") 10 
  liftIO $ putStrLn (mg5base ssetup)
  liftIO $ setCurrentDirectory (mg5base ssetup)
  liftIO $ putStrLn "createWorkDir" 
  workIOReadProcessWithExitCode ("bin/mg5") ["-f", sandboxdir ssetup </> "proc_card_mg5.dat"] ""
  liftIO $ system $ "bin/mg5 -f " ++ sandboxdir ssetup </> "proc_card_mg5.dat" 
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
getWorkDir = ask >>= \ws ->
             return (mcrundir (ws_ssetup ws) </> workname (ws_psetup ws))


-- | get string for me5_configuration.txt
me5confSetup :: (Model a) => WorkIO a String 
me5confSetup = do 
  ssetup <- ws_ssetup <$> ask 
  let tpath = runtmpldir ssetup 
      pydir = pythiapgsdir ssetup
  -- wdir <- getWorkDir
  templates <- liftIO $ directoryGroup tpath 
  return $ renderTemplateGroup
             templates
             [ ("pythiapgs", pydir ) ] 
             "me5_configuration.txt" 


-- | prepare for cards: param_card.dat, run_card.dat, pythia_card.dat 
--   and pgs_card.dat. Depending on LHESanitize, 
--   pythia_card.dat.sanitize and pgs_card.dat.sanitize are created. 
cardPrepare :: (Model a) => WorkIO a () 
cardPrepare = do 
  ws <- ask 
  let (ssetup,psetup,param,rsetup) = 
         ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
  wdir <- getWorkDir 
  let taskname = makeRunName psetup param rsetup 
  let carddir = wdir </> "Cards"
  debugMsgDef $ "prepare for cards for " ++ taskname
  checkDirectory wdir 10
  checkDirectory (wdir </> "SubProcesses") 10
  checkDirectory carddir 10   
  -- erase previous run 
  -- existThenRemove (carddir </> "me5_configuration.txt")
  existThenRemove (carddir </> "param_card.dat") 
  existThenRemove (carddir </> "run_card.dat") 
  existThenRemove (carddir </> "pythia_card.dat") 
  existThenRemove (carddir </> "pythia_card.dat.sanitize") 
  existThenRemove (carddir </> "pgs_card.dat")
  existThenRemove (carddir </> "pgs_card.dat.sanitize")
  -- 
  -- liftIO $ copyFile (runtmpldir ssetup </> "me5_configuration.txt" ) (carddir </> "me5_configuration.txt" )
  --
  -- me5conf <- me5confSetup 
  paramcard  <- liftIO $ paramCardSetup 
                           (modeltmpldir ssetup)
                           (model psetup)
                           param
  runcard    <- liftIO $ runCardSetup 
                           (runtmpldir ssetup)
                           (machine rsetup) 
                           (cut     rsetup) 
                           (match   rsetup) 
                           (rgrun   rsetup) 
                           (rgscale rsetup) 
		           (numevent rsetup) 
                           (hashSalt psetup) 
                           (setnum rsetup)
  pythiacard <- liftIO $ pythiaCardSetup 
                           (runtmpldir ssetup)
                           (match   rsetup)
                           (pythia  rsetup) 
  pgscard    <- liftIO $ pgsCardSetup
                           (runtmpldir ssetup)
                           (machine rsetup)
                           (pgs     rsetup) 
  -- 
  -- liftIO $ writeFile (carddir </> "me5_configuration.txt") me5conf 
  liftIO $ writeFile (carddir </> "param_card.dat") paramcard
  liftIO $ writeFile (carddir </> "run_card.dat")   runcard
  -- 
  pythiacard # 
    maybe (return ()) (\str -> 
      case lhesanitizer rsetup of 
        [] -> liftIO $ writeFile (carddir </> "pythia_card.dat") str
        _ -> liftIO $ writeFile (carddir </> "pythia_card.dat.sanitize") str
    )
  -- 
  pgscard # 
    maybe (return ()) (\str -> 
      case lhesanitizer rsetup of 
        [] -> liftIO $ writeFile (carddir </> "pgs_card.dat") str
        _ -> liftIO $ writeFile (carddir </> "pgs_card.dat.sanitize") str 
    )
  return () 

-- | 
generateEvents :: (Model a) => WorkIO a () 
generateEvents = do 
  ws <- ask 
  let (ssetup,psetup,param,rsetup) = 
         ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
  wdir <- getWorkDir
  let taskname = makeRunName psetup param rsetup 
  debugMsgDef $ "generating event for " ++ taskname
  liftIO $ setCurrentDirectory wdir 
  checkFile (wdir </> "Cards/run_card.dat") 10
  checkFile (wdir </> "Cards/param_card.dat") 10
  -- 
  case (pythia rsetup,lhesanitizer rsetup) of 
    (RunPYTHIA,[]) -> do 
      checkFile (wdir </> "Cards/pythia_card.dat") 10
    (RunPYTHIA,_:_) -> checkFile (wdir </> "Cards/pythia_card.dat.sanitize") 10
    (_,_) -> return () 
  -- 
  case lhesanitizer rsetup of 
    [] -> 
      case pgs rsetup  of 
        RunPGS _ -> checkFile (wdir </> "Cards/pgs_card.dat") 10
        NoPGS    -> return () 
    _:_ -> 
      case pgs rsetup of 
        NoPGS -> return ()
        _ -> checkFile (wdir </> "Cards/pgs_card.dat.sanitize") 10
  --  
  workIOReadProcessWithExitCode ("bin/generate_events") ["0", taskname] ""
  
  -- this is because madgraph-5-1.4 changes the file location. 
  let eventdir = wdir </> "Events" 
      unweightedevtfilename = taskname ++ "_unweighted_events.lhe" 
      rawunweightedevtfilename = "unweighted_events.lhe"
  liftIO $ setCurrentDirectory (eventdir</>taskname)
  liftIO $ system " ls -l " 
  checkFile (eventdir </> taskname </> rawunweightedevtfilename <.> "gz") 10 
  liftIO $ renameFile (eventdir</>taskname</>rawunweightedevtfilename<.>"gz") 
                      (eventdir</>taskname</>unweightedevtfilename<.>"gz")
  return ()

-- | 
sanitizeLHE :: (Model a) => WorkIO a () 
sanitizeLHE = do 
  ws <- ask 
  let (ssetup,psetup,param,rsetup) = 
         ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 

  -- WS _ssetup psetup param rsetup _storage <- ask 
  debugMsgDef "Start sanitizeLHE"
  case lhesanitizer rsetup of 
    [] -> throwError "ERROR: why did you call me? I am in sanitizeLHE." 
    cmds  -> do 
      wdir <- getWorkDir
      let eventdir = wdir </> "Events" 
          taskname = makeRunName psetup param rsetup 
          unweightedevtfilename = taskname ++ "_unweighted_events.lhe" 
          rawunweightedevtfilename = "unweighted_events.lhe"

      liftIO $ setCurrentDirectory (eventdir </> taskname)
      checkFile (eventdir </> taskname </> unweightedevtfilename <.> "gz") 10 

      debugMsgDef $ (eventdir </> taskname </> unweightedevtfilename <.> "gz")

      liftIO $ system ("gunzip -f " ++ unweightedevtfilename <.> "gz") 
      checkFile (eventdir </> taskname </> unweightedevtfilename) 10

      liftIO $ sanitize cmds unweightedevtfilename rawunweightedevtfilename

      case pythia rsetup of 
        RunPYTHIA -> return ()
        RunPYTHIA8 -> return ()
        NoPYTHIA -> do 
          liftIO $ system $ "gzip -f " ++ rawunweightedevtfilename
          liftIO $ renameFile (eventdir </> taskname </> rawunweightedevtfilename <.> "gz") 
                              (eventdir </> taskname </> unweightedevtfilename <.> "gz")


  return () 

-- | run PYTHIA as a user-defined process.
runPYTHIA :: (Model a) => WorkIO a () 
runPYTHIA = do
  ws <- ask 
  let (ssetup,psetup,param,rsetup) = 
         ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
  wdir <- getWorkDir 
  let bindir = wdir </> "bin"
      eventdir = wdir </> "Events" 
      carddir  = wdir </> "Cards"
      pythiadir = wdir </> "../pythia-pgs/src"
      taskname = makeRunName psetup param rsetup 
      unweightedevtfilename = taskname ++ "_unweighted_events.lhe" 
      rawunweightedevtfilename = "unweighted_events.lhe"
      fullhepfilename = taskname++"_pythia_events.hep"
      -- hepfilename = taskname++"_pythia_events.hep"
  liftIO $ renameFile (carddir </> "pythia_card.dat.sanitize") (carddir </> "pythia_card.dat")
  checkFile (eventdir</>taskname</> rawunweightedevtfilename) 10
  liftIO $ renameFile (eventdir </> taskname </> rawunweightedevtfilename) (eventdir</> rawunweightedevtfilename)
  liftIO $ setCurrentDirectory eventdir
  
  b <- liftIO $ doesFileExist rawunweightedevtfilename
  if b 
    then do 
      debugMsgDef "Start PYTHIA"
      (_,rmsg,_rerr) <- liftIO $ readProcessWithExitCode (bindir </> "internal" </> "run_pythia") [pythiadir] ""
      debugMsgDef rmsg
      checkFile (eventdir</>"pythia_events.hep") 10
      liftIO $ renameFile rawunweightedevtfilename (eventdir</>taskname</>unweightedevtfilename)
      liftIO $ setCurrentDirectory (eventdir</>taskname)
      liftIO $ system $ "gzip -f " ++ unweightedevtfilename
      liftIO $ renameFile (eventdir </> "pythia_events.hep") 
                          (eventdir</>taskname</>fullhepfilename)

      checkFile (unweightedevtfilename <.> "gz") 10
    else throwError "ERROR: No unweighted events" 
  return ()

-- | run PYTHIA as a user-defined process.
runPYTHIA8 :: (Model a) => WorkIO a () 
runPYTHIA8 = do
  ws <- ask 
  let (ssetup,psetup,param,rsetup) = 
         ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
  wdir <- getWorkDir 
  let bindir = wdir </> "bin"
      eventdir = wdir </> "Events" 
      carddir  = wdir </> "Cards"
      pythiadir = wdir </> "../pythia-pgs/src"
      taskname = makeRunName psetup param rsetup 
      unweightedevtfilename = taskname ++ "_unweighted_events.lhe" 
      rawunweightedevtfilename = "unweighted_events.lhe"
      fullhepfilename = taskname++"_pythia_events.hep"
      hepevtfilename = "pythia_event.hepevt"
      stdhepresult = "pythia_event.hep"
  liftIO $ putStrLn "Start PYTHIA8"
 
  checkFile (eventdir</>taskname</> rawunweightedevtfilename) 10
  liftIO $ setCurrentDirectory (eventdir </> taskname)
  
  liftIO $ setEnv "PYTHIA8DATA" (pythia8dir ssetup </> "xmldoc") True
  
  debugMsgDef "Start PYTHIA8"
  (_,rmsg,_rerr) <- 
    liftIO $ readProcessWithExitCode (pythia8toHEPEVT ssetup) [rawunweightedevtfilename,hepevtfilename]  ""
  debugMsgDef rmsg
  (_,rmsg2,_rerr) <- 
    liftIO $ readProcessWithExitCode (hepevt2stdhep ssetup) [hepevtfilename,stdhepresult]  ""
  debugMsgDef rmsg2 
  liftIO $ renameFile stdhepresult fullhepfilename
  liftIO $ renameFile rawunweightedevtfilename unweightedevtfilename
  liftIO $ system $ "gzip -f " ++ unweightedevtfilename
  liftIO $ removeFile hepevtfilename
  return ()


-- | 
runPGS :: (Model a) => WorkIO a () 
runPGS = do
  ws <- ask 
  let (ssetup,psetup,param,rsetup) = 
         ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
  wdir <- getWorkDir 
  let eventdir = wdir </> "Events" 
      taskname = makeRunName psetup param rsetup 
      pgsdir   = wdir </> "../pythia-pgs/src"
      carddir  = wdir </> "Cards"
      fullhepfilename = taskname ++ "_pythia_events.hep"
      hepfilename = "pythia_events.hep"
      uncleanedfilename = "pgs_uncleaned.lhco"
  liftIO $ setCurrentDirectory (eventdir</>taskname)
  checkFile (carddir </> "pgs_card.dat.sanitize") 10 
  liftIO $ renameFile (carddir </> "pgs_card.dat.sanitize") (carddir </> "pgs_card.dat")
  let pythiaresult = hepfilename 
  -- 
  withTempFile (eventdir</>taskname</>fullhepfilename) 
               (eventdir</>taskname</>pythiaresult) $ do 
    checkFile (eventdir</>taskname</>pythiaresult) 10
    debugMsgDef "Start pgs"
    (rmsg,rerr) <- liftIO $ do 
      putEnv  $ "PDG_MASS_TBL=" ++ pgsdir </> "mass_width_2004.mc "
      (_,rmsg,rerr) <- readProcessWithExitCode (pgsdir </> "pgs") ["--stdhep",pythiaresult,"--nev","0","--detector","../../Cards/pgs_card.dat",uncleanedfilename] "" 
      return (rmsg,rerr)
    -- debugMsgDef rmsg 
    -- debugMsgDef rerr 
    return ()
-- | 
runClean :: (Model a) => WorkIO a () 
runClean = do
  ws <- ask 
  let (ssetup,psetup,param,rsetup) = 
         ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
  wdir <- getWorkDir 
  let eventdir = wdir </> "Events" 
      pgsdir   = wdir </> "../pythia-pgs/src"
      taskname = makeRunName psetup param rsetup 
      uncleanedfilename = "pgs_uncleaned.lhco"
      cleanedfilename = "pgs_cleaned.lhco"
      finallhco = taskname ++ "_pgs_events.lhco"
  liftIO $ setCurrentDirectory (eventdir</>taskname)
  checkFile (eventdir</>taskname</>uncleanedfilename) 10
  debugMsgDef "Start clean_output"
  workIOReadProcessWithExitCode (pgsdir </> "clean_output") [ "-muon", uncleanedfilename, cleanedfilename ] "" 
  liftIO $ renameFile (eventdir</>taskname</>cleanedfilename) (eventdir</>taskname</>finallhco)
  liftIO $ system ("gzip -f " ++ finallhco) 
  return ()

{-
-- | 
updateBanner :: (Model a) => WorkIO a () 
updateBanner = do
  WS _ssetup psetup rsetup _ <- ask 
  wdir <- getWorkDir 
  case (usercut rsetup) of 
    NoUserCutDef -> return () 
    UserCutDef uc -> do  
      let eventdir = wdir </> "Events" 
          taskname = makeRunName psetup param rsetup 
          carddir  = wdir </> "Cards"
          bannerfilename = taskname ++ "_banner.txt"
          newbannerfilename = taskname ++ "_newbanner.txt"
          usercutcontent = prettyprintUserCut uc
      liftIO $ setCurrentDirectory (eventdir</>taskname)
      checkFile (eventdir</>taskname</>bannerfilename) 10
      bannerstr  <- liftIO $ readFile (eventdir</>taskname</>bannerfilename)
      pgscardstr <- liftIO $ readFile (carddir </> "pgs_card.dat")  
      let newbannerstr = bannerstr ++ usercutcontent ++ pgscardstr
      liftIO $ writeFile (eventdir</>taskname</>newbannerfilename) newbannerstr 
-}

renamePythiaPGSResult :: Model a => WorkIO a () 
renamePythiaPGSResult = do 
  ws <- ask 
  let (ssetup,psetup,param,rsetup) = 
         ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
  wdir <- getWorkDir 
  let taskname = makeRunName psetup param rsetup 
      eventdir = wdir </> "Events" 
      taskdir = eventdir </> taskname 
      ohep = "fermi_pythia_events.hep.gz"
      nhep = taskname++"_pythia_events.hep.gz"
      olhe = "fermi_pythia_events.lhe.gz"
      nlhe = taskname ++ "_pythia_events.lhe.gz"
      olhco = "fermi_pgs_events.lhco.gz"
      nlhco = taskname ++ "_pgs_events.lhco.gz"
      opylog = "fermi_pythia.log"
      npylog = taskname ++ "_pythia.log"
      opgslog= "fermi_pgs.log"
      npgslog = taskname ++ "_pgs.log"
      existThenRename x y = do
        b <- liftIO $ doesFileExist (taskdir </> x )
        if b
          then do liftIO $ renameFile (taskdir </> x) (taskdir </> y) 
                  checkVetoFile (taskdir </> x) 3
          else return ()
  existThenRename ohep nhep 
  existThenRename olhe nlhe
  existThenRename olhco nlhco 
  existThenRename opylog npylog
  existThenRename opgslog npgslog 


-- |
cleanHepFiles :: (Model a) => WorkIO a () 
cleanHepFiles = do 
  ws <- ask 
  let (ssetup,psetup,param,rsetup) = 
         ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
  wdir <- getWorkDir 
  let taskname = makeRunName psetup param rsetup 
      eventdir = wdir </> "Events" 
      existThenRemoveForAny x = existThenRemove (eventdir</>taskname</> x)
      clean = mapM_ existThenRemoveForAny  
      hepfilename = taskname++"_pythia_events.hep"
      hepfilename2 = "pythia_events.hep"
      hepfilename3 = "fermi_pythia_events.hep" 
      hepgzfilename = taskname++"_pythia_events.hep.gz"
      hepgzfilename2 = "pythia_events.hep.gz"
      hepgzfilename3 = "fermi_pythia_events.hep.gz" 
      hepevtfilename = "afterusercut.hepevt"  
      stdhepfilename = "afterusercut.stdhep"      
      uncleanedfilename = "pgs_uncleaned.lhco"
      cleanedfilename = "pgs_cleaned.lhco"
      lhefile = "events.lhe.gz"
      onlyhep = [ hepfilename ] 
      allhep  = [ lhefile 
                , hepfilename
                , hepfilename2
                , hepfilename3 
                , hepgzfilename
                , hepgzfilename2
                , hepgzfilename3 
                , hepevtfilename
                , stdhepfilename
                , uncleanedfilename
                , cleanedfilename ]
  liftIO $ sleep 5
  clean allhep 

-- | 
cleanAll :: (Model a) => WorkIO a () 
cleanAll = do 
  ws <- ask 
  let (ssetup,psetup,param,rsetup) = 
         ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
  wdir <- getWorkDir 
  let taskname = makeRunName psetup param rsetup 
      eventdir = wdir </> "Events" 
      existThenRemoveForAny x = existThenRemove (eventdir</>taskname</>x)
      clean = mapM_ existThenRemoveForAny  
      hepfilename = taskname++"_pythia_events.hep"
      hepevtfilename = "afterusercut.hepevt"  
      stdhepfilename = "afterusercut.stdhep"      
      uncleanedfilename = "pgs_uncleaned.lhco"
      cleanedfilename = "pgs_cleaned.lhco"
      bannerfile    = taskname++ "_fermi_banner.txt"
      treefile1     = taskname++ "_beforeveto.tree.gz"
      lheeventfile1 = taskname ++ "_events.lhe.gz"
      treefile2     = taskname ++ "_events.tree.gz"
      newbannerfile = taskname ++ "_fermi_newbanner.txt"
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
  b <- liftIO $ doesDirectoryExist ( eventdir</>taskname</>pythiadir)
  if b 
    then do
      liftIO $ setCurrentDirectory ( eventdir</>taskname</>pythiadir ) 
      liftIO $ system "rm *"
      liftIO $ setCurrentDirectory ( eventdir</>taskname )
      liftIO $ removeDirectory (eventdir</>taskname</>pythiadir )
    else return () 

-- |      
makeHepGz :: (Model a) => WorkIO a () 
makeHepGz = do 
  ws <- ask 
  let (ssetup,psetup,param,rsetup) = 
         ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
  wdir <- getWorkDir 
  let taskname = makeRunName psetup param rsetup 
      eventdir = wdir </> "Events" 
      hepfilename = taskname++"_pythia_events.hep"
  liftIO $ setCurrentDirectory (eventdir</>taskname)
  b <- liftIO $ doesFileExist (hepfilename <.> "gz")
  if b 
    then return () 
    else case (pythia rsetup, match rsetup, uploadhep rsetup) of 
           (_,MLM,UploadHEP) -> do 
              checkFile hepfilename 10 
              liftIO $ system $ "gzip -f " ++ hepfilename 
              return ()
           (RunPYTHIA,_,UploadHEP) -> do 
              checkFile hepfilename 10 
              liftIO $ system $ "gzip -f " ++ hepfilename
              return () 
           _ -> return () 
  liftIO $ threadDelay 5000000
  return ()

