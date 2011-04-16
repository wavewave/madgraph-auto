{-# LANGUAGE PackageImports, TypeFamilies, FlexibleInstances, FlexibleContexts #-}

module HEP.Automation.MadGraph.SetupType where

import Control.Monad.Reader

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.Cluster
import HEP.Automation.MadGraph.UserCut

import HEP.Storage.WebDAV.Type

data ScriptSetup = SS { 
    templatedir :: String
  , workingdir  :: String 
  , mg5base    :: String
  , workbase   :: String
} deriving Show

data ProcessSetup a = PS { 
    mversion :: MadGraphVersion
  , model   :: a
  , process :: String
  , processBrief :: String  
  , workname :: String 
  } deriving Show

data RunSetup a = RS { 
    param   :: ModelParam a
  , numevent :: Int
  , machine :: MachineType
  , rgrun   :: RGRunType
  , rgscale :: Double
  , match   :: MatchType
  , cut     :: CutType
  , pythia  :: PYTHIAType
  , usercut :: UserCutSet
  , pgs     :: PGSType 
  , setnum  :: Int 
} --  deriving Show

instance (Model a) => Show (RunSetup a) where
  show (RS pa nu ma rgr rgs mat cu py us pg es) = 
    show "RS " ++ show pa ++ ":" ++ show nu ++ ":" ++ show ma ++ ":" 
         ++ show rgr ++ ":" ++ show rgs ++ ":" ++ show mat ++ ":"
         ++ show cu ++ ":" ++ show py ++ ":" ++ show us ++ ":" 
         ++ show pg ++ ":" ++ show es


data ClusterSetup = CS { 
  cluster :: ClusterRunType
} deriving Show


data WorkSetup a = WS { 
  ws_ssetup :: ScriptSetup, 
  ws_psetup :: ProcessSetup a, 
  ws_rsetup :: RunSetup a, 
  ws_csetup :: ClusterSetup, 
  ws_storage :: WebDAVRemoteDir
} deriving Show

type WorkIO b a = ReaderT (WorkSetup b) IO a 
