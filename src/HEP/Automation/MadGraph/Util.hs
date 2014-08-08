{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.Util 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module HEP.Automation.MadGraph.Util where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader 
import Control.Monad.Error
import System.Directory
import System.Process
import System.Exit
import System.Posix.Files
--
import HEP.Automation.MadGraph.Log
import HEP.Automation.MadGraph.Model 
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type

-- | 
workIOReadProcessWithExitCode :: FilePath -> [String] -> String -> WorkIO a (ExitCode,String,String)
workIOReadProcessWithExitCode cmd args input = do 
  (ex, out, err) <- liftIO $ readProcessWithExitCode cmd args input
  debugMsgDef out
  debugMsgDef err
  case ex of 
    ExitSuccess -> return (ex,out,err)
    ExitFailure c -> throwError $ "error exit code = " ++ show c ++ " while running " ++ cmd ++ " " ++ show args 
                                  ++ "\n stdout = " ++ out 

-- | 
checkFile :: (Model a) => FilePath -> Int -> WorkIO a () 
checkFile fp n = do 
  if n < 0 
    then throwError $ "no " ++ fp ++ " ever created." 
    else do 
      b <- liftIO $ doesFileExist fp 
      if b 
        then return () 
        else do 
          debugMsgDef $ fp ++ " not exist : " ++ show (n-1) ++ " chances left" 
          liftIO $ threadDelay 5000000 
          checkFile fp (n-1)   
{-      if b  
        then do 
          b2 <- liftIO $ getFileStatus fp >>= return.(> (0 :: Int)).fromIntegral.fileSize
          if b2 
            then do 
              debugMsgDef $ "nontrivial " ++ fp ++ " exists" 
              (ex,str,err) <- liftIO $ readProcessWithExitCode "/usr/sbin/lsof" [fp] "" 
              case ex of 
                ExitSuccess -> do 
                  debugMsgDef $ "some program is using the file " ++ fp  
                  debugMsgDef str
                  liftIO $ threadDelay 5000000 
                  checkFile fp (n-1)
                ExitFailure 1 -> do 
                  debugMsgDef $ "okay. it's safe to use this file"
                  return () 
                _ -> do 
                  debugMsgDef "mmm? what's going on here?"
                  debugMsgDef $ show ex
                  debugMsgDef $ str 
                  debugMsgDef $ err
                  liftIO $ threadDelay 5000000
                  checkFile fp (n-1)

            else do { liftIO (threadDelay 5000000); checkFile fp (n-1) } 
          else do  
            debugMsgDef $ fp ++ " not exist : " ++ show (n-1) ++ " chances left" 
            liftIO $ threadDelay 5000000 
            checkFile fp (n-1)   -}

-- | 
checkVetoFile :: (Model a) => FilePath -> Int -> WorkIO a () 
checkVetoFile fp n = do 
  if n < 0 
    then throwError $ fp ++ " still exists "
    else do 
      b <- liftIO $ doesFileExist fp 
      if not b 
        then do { debugMsgDef (fp ++ " is not exist. good"); return ()}
        else do { liftIO $ threadDelay 5000000; checkVetoFile fp (n-1) }

-- | 
existThenRemove :: (Model a) => FilePath -> WorkIO a () 
existThenRemove fp = do 
  b <- liftIO $ doesFileExist fp 
  if b 
    then do { liftIO $ removeFile fp; checkVetoFile fp 3 }  
    else return () 

-- | 
checkDirectory :: (Model a) => FilePath -> Int -> WorkIO a () 
checkDirectory fp n = do 
  if n < 0 
    then throwError $ "no " ++ fp ++ " ever created." 
    else do 
      b <- liftIO $ doesDirectoryExist fp 
      if b  
         then do { debugMsgDef (fp ++ " checked") ; return () } 
         else do { liftIO $ threadDelay 5000000 ; checkDirectory fp (n-1) }  

-- | 
makeRunName :: (Model a) => ProcessSetup a -> ModelParam a -> RunSetup -> String 
makeRunName psetup param rsetup = 
  let mprefix = briefShow (model psetup)  
      masscoup = briefParamShow param 
      machineName = case (machine rsetup) of 
        TeVatron -> "TeVa" 
        LHC7 detector -> "LHC7" ++ show detector
        LHC8 detector -> "LHC8" ++ show detector
        LHC10 detector -> "LHC10" ++ show detector
        LHC10 detector -> "LHC13" ++ show detector
        LHC14 detector -> "LHC14" ++ show detector
        Parton be detector -> "Ptn" ++ (show be) ++ (show detector)
        PolParton be ipol detector -> 
          "PolPtn" ++ (show be) 
          ++ "First"  ++ (show . rhpol_percent . particle1pol) ipol
          ++ "Second" ++ (show . rhpol_percent . particle2pol) ipol  
          ++ (show detector)
      matchName = case (match rsetup) of 
        MLM -> "MLM"
        NoMatch -> "NoMatch"
      cutName = case (cut rsetup) of 
        NoCut -> "NoCut"
        DefCut -> "DefCut"
        KCut -> "KCut"
      pgsName = case (pgs rsetup) of
        NoPGS -> "" 
        RunPGS (jetalgo,tau) ->
          let taustr = case tau of 
                WithTau -> "_WithTau"
                -- NoTau   -> "_NoTau" 
              jetalgoName = case jetalgo of
                Cone conesize -> "Cone" ++ show conesize
                KTJet conesize -> "KT" ++ show conesize
                AntiKTJet conesize -> "AntiKT" ++ show conesize
          in jetalgoName ++ taustr 
  in  mprefix++masscoup
      ++"_"++processBrief psetup
      ++"_"++machineName
      ++"_"++matchName
      ++"_"++cutName
      ++"_"++pgsName
      ++"_Set" ++ show (setnum rsetup)  

-- | 
naming :: (Model a) => WorkSetup a -> String 
naming = makeRunName <$> ws_psetup <*> ws_param <*> ws_rsetup 

-- | 

withTempFile :: MonadIO m => FilePath -> FilePath -> m r -> m r
withTempFile ofp tfp act = do
  liftIO $ renameFile ofp tfp 
  r <- act 
  liftIO $ renameFile tfp ofp 
  return r

