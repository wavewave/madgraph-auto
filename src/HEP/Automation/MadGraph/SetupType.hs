{-# LANGUAGE PackageImports #-}

module HEP.Automation.MadGraph.SetupType where

import "mtl" Control.Monad.Reader

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.Cluster
import HEP.Automation.MadGraph.UserCut

data ScriptSetup = SS { 
    scriptbase :: String
  , mg5base    :: String
  , workbase   :: String
}

data ProcessSetup = PS { 
    mversion :: ModelVersion
  , model   :: Model
  , process :: String
  , processBrief :: String  
  , workname :: String 
  }

data RunSetup = RS { 
    param   :: Param
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
}

data ClusterSetup = CS { 
  cluster :: ClusterRunType
}

data WorkSetup = WS { 
  ws_ssetup :: ScriptSetup, 
  ws_psetup :: ProcessSetup, 
  ws_rsetup :: RunSetup, 
  ws_csetup :: ClusterSetup
} 

type WorkIO a = ReaderT WorkSetup IO a 
