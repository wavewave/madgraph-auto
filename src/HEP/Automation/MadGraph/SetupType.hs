{-# LANGUAGE PackageImports, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.SetupType 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- types for specifying a pipeline job of madgraph
-- 
-----------------------------------------------------------------------------

module HEP.Automation.MadGraph.SetupType where

-- from others 
import Control.Monad.Error
import Control.Monad.Reader
import Data.Data
-- from hep-platform
import HEP.Storage.WebDAV.Type
-- from this package
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.UserCut

-- | 
data ScriptSetup = 
  SS { modeltmpldir :: String  -- ^ where model paramcard located, previously templatedir 
     , runtmpldir   :: String  -- ^ where runcard and other templates located, previously templatedir 
     , sandboxdir   :: String  -- ^ where we do some temporary file works, previously workdir 
     , mg5base      :: String  -- ^ where mg5 main installation
     , mcrundir     :: String  -- ^ where montecarlo run work directory located, previously workbase 
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

data LHESanitizerType = NoLHESanitize 
                      | LHESanitize [Int]  
                      deriving (Show,Typeable,Data)

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
  , lhesanitizer :: LHESanitizerType
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
  show (RS pa nu ma rgr rgs mat cu py us ls pg ja hu es) = 
    "Run:" ++ show pa ++ ":" ++ show nu ++ ":" ++ show ma ++ ":" 
          ++ show rgr ++ ":" ++ show rgs ++ ":" ++ show mat ++ ":"
          ++ show cu ++ ":" ++ show py ++ ":" ++ show us ++ ":" ++ show ls ++ ":" 
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

