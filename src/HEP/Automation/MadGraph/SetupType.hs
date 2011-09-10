{-# LANGUAGE PackageImports, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module HEP.Automation.MadGraph.SetupType where

import Control.Monad.Reader
import Control.Monad.Error

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.UserCut

import HEP.Storage.WebDAV.Type

import Data.Typeable
import Data.Data

data ScriptSetup = SS { 
    templatedir :: String
  , workingdir  :: String 
  , mg5base    :: String
  , workbase   :: String
} deriving (Show,Typeable,Data)

data ProcessSetup a = PS { 
    model   :: a
  , process :: String
  , processBrief :: String  
  , workname :: String 
  } 

deriving instance Typeable1 ProcessSetup  
deriving instance (Model a) => Data (ProcessSetup a)

-- deriving (Typeable,Data)

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
  , jetalgo :: PGSJetAlgorithm
  , uploadhep :: HEPFileType
  , setnum  :: Int 
} 

deriving instance Typeable1 RunSetup  
deriving instance (Model a) => Data (RunSetup a)


data SMPConfiguration = SingleCPU | MultiCPU Int
  deriving (Show,Typeable,Data)

instance (Model a) => Show (ProcessSetup a) where
  show (PS mdl pr prb wk ) = 
    "Process:" ++ modelName mdl ++ ":"
               ++ pr ++ ":" ++ prb ++ ":" ++ wk ++ "|"

instance (Model a) => Show (RunSetup a) where
  show (RS pa nu ma rgr rgs mat cu py us pg ja hu es) = 
    "Run:" ++ show pa ++ ":" ++ show nu ++ ":" ++ show ma ++ ":" 
          ++ show rgr ++ ":" ++ show rgs ++ ":" ++ show mat ++ ":"
          ++ show cu ++ ":" ++ show py ++ ":" ++ show us ++ ":" 
          ++ show pg ++ ":" ++ show ja ++ ":" ++ show hu ++ ":" 
          ++  show es ++ "|"


data ClusterSetup a = CS { 
  cluster :: ClusterRunType a
} deriving (Show,Typeable,Data)


data WorkSetup a = WS { 
  ws_ssetup :: ScriptSetup, 
  ws_psetup :: ProcessSetup a, 
  ws_rsetup :: RunSetup a, 
  ws_csetup :: ClusterSetup a, 
  ws_storage :: WebDAVRemoteDir
} deriving (Show,Typeable,Data)

type WorkIO b a = ErrorT String (ReaderT (WorkSetup b) IO) a 

data ClusterRunType a = NoParallel 
                      | Parallel Int 
                      | Cluster { 
                          cluster_masterwork :: WorkSetup a, 
                          cluster_workname   :: FilePath
                          } deriving (Typeable,Data)

instance Show (ClusterRunType a) where
  show NoParallel = "NoParallel"
  show (Parallel i) = "Parallel " ++ show i 
  show (Cluster _ _f) = "Cluster"
  
  
defaultClusterNamingFunction :: WorkSetup a -> WorkSetup a -> FilePath
defaultClusterNamingFunction masterws ws =
  let wn = workname . ws_psetup $ masterws  
      snum = setnum .  ws_rsetup $ ws
  in  wn ++ "Cluster" ++ show snum
      

      
data ClusterWork a = ClusterWork { 
  master :: WorkSetup a, 
  slaves :: [WorkSetup a] 
  }

