{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.SetupType 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
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
import Data.List
-- from hep-platform
import HEP.Storage.WebDAV.Type
-- from this package
import HEP.Automation.MadGraph.Card
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Type

-- | 
data ScriptSetup = 
  SS { modeltmpldir :: String  -- ^ where model paramcard located, previously templatedir 
     , runtmpldir   :: String  -- ^ where runcard and other templates located, previously templatedir 
     , sandboxdir   :: String  -- ^ where we do some temporary file works, previously workdir 
     , mg5base      :: String  -- ^ where mg5 main installation
     , mcrundir     :: String  -- ^ where montecarlo run work directory located, previously workbase 
     , pythia8toHEPEVT :: FilePath -- ^ pythia8toHEPEVT script path
     , hepevt2stdhep :: FilePath   -- ^ hepevt2stdhep script path 
     } deriving (Show,Typeable,Data)

data ProcessSetup a = PS { 
    model   :: a
  , process :: MGProcess 
  , processBrief :: String  
  , workname :: String 
  , hashSalt :: HashSalt 
  } 

deriving instance Typeable1 ProcessSetup  
deriving instance (Model a) => Data (ProcessSetup a)

-- deriving (Typeable,Data)


data RunSetup = 
    RS { numevent :: Int
       , machine :: MachineType
       , rgrun   :: RGRunType
       , rgscale :: Double
       , match   :: MatchType
       , cut     :: CutType
       , pythia  :: PYTHIAType
       , lhesanitizer :: LHESanitizerType
       , pgs     :: PGSType 
       , uploadhep :: HEPFileType
       , setnum  :: Int 
       } 

deriving instance Typeable RunSetup  
deriving instance Data RunSetup


instance (Model a) => Show (ProcessSetup a) where
  show (PS mdl (MGProc _ pr) prb wk _ ) = 
    "Process:" ++ modelName mdl ++ ":"
               ++ intercalate "/" pr ++ ":" ++ prb ++ ":" ++ wk ++ "|"


instance Show RunSetup where
  show (RS nu ma rgr rgs mat cu py ls pg hu es) = 
    "Run:" ++ show nu ++ ":" ++ show ma ++ ":" 
          ++ show rgr ++ ":" ++ show rgs ++ ":" ++ show mat ++ ":"
          ++ show cu ++ ":" ++ show py ++ ":" ++ show ls ++ ":" 
          ++ show pg ++ ":" ++ show hu ++ ":" 
          ++  show es ++ "|"


data WorkSetup a = WS { 
  ws_ssetup :: ScriptSetup, 
  ws_psetup :: ProcessSetup a, 
  ws_param  :: ModelParam a, 
  ws_rsetup :: RunSetup, 
  ws_storage :: WebDAVRemoteDir
} 

deriving instance (Model a) => Show (WorkSetup a)
deriving instance Typeable1 WorkSetup 
deriving instance (Model a) => Data (WorkSetup a)

type WorkIO b a = ErrorT String (ReaderT (WorkSetup b) IO) a 

