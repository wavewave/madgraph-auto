{-# LANGUAGE ScopedTypeVariables #-}

module Fortran where

import System.IO
import System.Process
import System.Directory
import System.Posix.Unistd (sleep)

import Control.Monad

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Text.Printf

import Work 

data UserCut = UserCut {
    uc_metcut    :: Double 
  , uc_etacutlep :: Double 
  , uc_etcutlep  :: Double
  , uc_etacutjet :: Double
  , uc_etcutjet  :: Double  
}


hep2lheSetup :: FilePath -> UserCut -> IO String 
hep2lheSetup fp uc = do 
  templates <- directoryGroup fp 
  return $ (renderTemplateGroup 
              templates
              [ ("metcut"      , show (uc_metcut    uc))
	      , ("etacutlep"   , show (uc_etacutlep uc))
              , ("etcutlep"    , show (uc_etcutlep  uc))
              , ("etacutjet"   , show (uc_etacutjet uc))
              , ("etcutjet"    , show (uc_etcutjet  uc)) ] 
              "hep2lhe.f") ++ "\n\n\n" 


compileFortran :: ScriptSetup -> UserCut -> IO ()
compileFortran ssetup uc = do 
  putStrLn $ "set up a working directory" 
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





  
