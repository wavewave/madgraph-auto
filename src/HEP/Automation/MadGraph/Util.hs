{-# LANGUAGE ScopedTypeVariables #-}

-- This is V3
module HEP.Automation.MadGraph.Util where

import HEP.Automation.MadGraph.Model 
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.SetupType

import qualified Data.ByteString.Char8  as B
import Crypto.Classes
import Data.Digest.Pure.MD5 

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader 
import Control.Monad.Error

import System.Directory
import System.Process
import System.Exit
import System.Posix.Files


workIOReadProcessWithExitCode :: FilePath -> [String] -> String -> WorkIO a ()
workIOReadProcessWithExitCode cmd args input = do 
  (ex, out, err) <- liftIO $ readProcessWithExitCode cmd args input
  liftIO $ putStrLn out
  liftIO $ putStrLn err
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
          b2 <- liftIO $ getFileStatus fp >>= return.(> (0 :: Int)).fromIntegral.fileSize
          if b2 then liftIO $ do { putStrLn $ fp ++ " checked" ; return () } 
                else do { liftIO (threadDelay 5000000); checkFile fp (n-1) } 
          else do  
            liftIO $ putStrLn $ fp ++ " not exist : " ++ show (n-1) ++ " chances left" 
            liftIO $ threadDelay 5000000 
            checkFile fp (n-1)  

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

makeRunName :: (Model a) => ProcessSetup a -> RunSetup a -> String 
makeRunName psetup rsetup = 
  let mprefix = briefShow (model psetup)  
      masscoup = briefParamShow (param rsetup) 
      machineName = case (machine rsetup) of 
        TeVatron -> "TeVa" 
        LHC7 detector -> "LHC7" ++ show detector
        LHC14 detector -> "LHC14" ++ show detector
        Parton be detector -> "Ptn" ++ (show be) ++ (show detector)
      matchName = case (match rsetup) of 
        MLM -> "MLM"
        NoMatch -> "NoMatch"
      cutName = case (cut rsetup) of 
        NoCut -> "NoCut"
        DefCut -> "DefCut"
        KCut -> "KCut"
      pgsName = case (pgs rsetup) of
        RunPGSNoTau -> "_NoTau"
        _           -> "" 
      jetalgoName = case (jetalgo rsetup) of
        Cone conesize -> "Cone" ++ show conesize
        KTJet conesize -> "KT" ++ show conesize
        AntiKTJet conesize -> "AntiKT" ++ show conesize
  in  mprefix++masscoup++"_"++processBrief psetup
        ++"_"++machineName++"_"++matchName++"_"++cutName++pgsName
        ++"_"++jetalgoName ++"_Set" ++ show (setnum rsetup)  

naming :: (Model a) => WorkSetup a -> String 
naming = makeRunName <$> ws_psetup <*>  ws_rsetup 

md5naming :: (Model a) => WorkSetup a -> String
md5naming ws = let md5str :: MD5Digest = hash' . B.pack . naming $ ws   
               in  "temp" ++ show md5str 


