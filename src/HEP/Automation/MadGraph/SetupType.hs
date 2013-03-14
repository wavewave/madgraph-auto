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
-- from hep-platform
import HEP.Storage.WebDAV.Type
-- from this package
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.Model
-- import HEP.Automation.MadGraph.UserCut

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

data SanitizeType = Elim [Int] | Replace [(Int,Int)]
                  deriving (Show,Typeable,Data)

data LHESanitizerType = NoLHESanitize 
                      | LHESanitize SanitizeType   
                      deriving (Show,Typeable,Data)

--     param   :: ModelParam a


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
       -- , jetalgo :: PGSJetAlgoNTau
       , uploadhep :: HEPFileType
       , setnum  :: Int 
       } 



-- | this is RunSetup without parameter
--   I am going to replace RunSetup with this soon.

{-
data MGRunSetup = MGRS { 
    mgrs_numevent :: Int
  , mgrs_machine :: MachineType
  , mgrs_rgrun   :: RGRunType
  , mgrs_rgscale :: Double
  , mgrs_match   :: MatchType
  , mgrs_cut     :: CutType
  , mgrs_pythia  :: PYTHIAType
  , mgrs_lhesanitizer :: LHESanitizerType
  , mgrs_pgs     :: PGSType 
  -- , mgrs_jetalgo :: PGSJetAlgoNTau
  , mgrs_uploadhep :: HEPFileType
  , mgrs_setnum  :: Int 
} -}

{-

mGRunSetup2RunSetup ::  ModelParam a -> MGRunSetup -> RunSetup a
mGRunSetup2RunSetup p MGRS {..}  = 
  RS { param = p 
     , numevent = mgrs_numevent
     , machine = mgrs_machine
     , rgrun = mgrs_rgrun 
     , rgscale = mgrs_rgscale
     , match = mgrs_match 
     , cut = mgrs_cut 
     , pythia = mgrs_pythia
     , lhesanitizer = mgrs_lhesanitizer
     , pgs = mgrs_pgs 
     -- , jetalgo = mgrs_jetalgo 
     , uploadhep = mgrs_uploadhep
     , setnum = mgrs_setnum 
     } 

-}

deriving instance Typeable RunSetup  
deriving instance Data RunSetup


instance (Model a) => Show (ProcessSetup a) where
  show (PS mdl pr prb wk ) = 
    "Process:" ++ modelName mdl ++ ":"
               ++ pr ++ ":" ++ prb ++ ":" ++ wk ++ "|"

-- instance (Model a) => Show (ModelParam a) where
--   show pa =  


instance Show RunSetup where
  show (RS nu ma rgr rgs mat cu py ls pg hu es) = 
    "Run:" ++ show nu ++ ":" ++ show ma ++ ":" 
          ++ show rgr ++ ":" ++ show rgs ++ ":" ++ show mat ++ ":"
          ++ show cu ++ ":" ++ show py ++ ":" ++ show ls ++ ":" 
          ++ show pg ++ ":" ++ show hu ++ ":" 
          ++  show es ++ "|"

{-
instance Show MGRunSetup where
  show (MGRS nu ma rgr rgs mat cu py ls pg hu es) = 
    "MGRun:" ++ show nu ++ ":" ++ show ma ++ ":" 
          ++ show rgr ++ ":" ++ show rgs ++ ":" ++ show mat ++ ":"
          ++ show cu ++ ":" ++ show py ++ ":" ++ show ls ++ ":" 
          ++ show pg ++ ":" ++ show hu ++ ":" 
          ++  show es ++ "|"
-}

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

