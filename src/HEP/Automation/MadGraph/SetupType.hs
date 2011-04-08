{-# LANGUAGE PackageImports, TypeFamilies, FlexibleInstances, FlexibleContexts #-}

module HEP.Automation.MadGraph.SetupType where

import "mtl" Control.Monad.Reader

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.Cluster
import HEP.Automation.MadGraph.UserCut

data ScriptSetup = SS { 
    templatedir :: String
  , workingdir  :: String 
  , mg5base    :: String
  , workbase   :: String
} deriving Show

data (Model a) => ProcessSetup a = PS { 
    mversion :: MadGraphVersion
  , model   :: a
  , process :: String
  , processBrief :: String  
  , workname :: String 
  } deriving Show

data (Model a) => RunSetup a = RS { 
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
} -- deriving Show

data ClusterSetup = CS { 
  cluster :: ClusterRunType
} deriving Show

data (Model a) => WorkSetup a = WS { 
  ws_ssetup :: ScriptSetup, 
  ws_psetup :: ProcessSetup a, 
  ws_rsetup :: RunSetup a, 
  ws_csetup :: ClusterSetup
} -- deriving Show

type WorkIO b a = ReaderT (WorkSetup b) IO a 
