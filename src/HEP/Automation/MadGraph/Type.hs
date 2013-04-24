{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.Type 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- types for madgraph
-- 
-----------------------------------------------------------------------------

module HEP.Automation.MadGraph.Type where

import           Data.Typeable
import           Data.Data

newtype HashSalt = HashSalt { unHashSalt :: Maybe Int } 
                 deriving (Show,Typeable,Data)       
 
data MGProcess = MGProc { mgp_definelines :: [String] 
                        , mgp_processes   :: [String] }
                 deriving (Show,Typeable,Data)        


data SanitizeType = Elim [Int] | Replace [(Int,Int)]
                  deriving (Show,Typeable,Data)

data LHESanitizerType = NoLHESanitize 
                      | LHESanitize SanitizeType   
                      deriving (Show,Typeable,Data)

-- | 
newtype Polarization = RH { rhpol_percent  :: Double }
                     deriving (Show,Typeable,Data)

-- | 
data InitPolarization = InitPolarization 
                        { particle1pol :: Polarization
                        , particle2pol :: Polarization
                        }  
                        deriving (Show,Typeable,Data)

-- | 
data Detector = Tevatron | LHC | CMS | ATLAS
              deriving (Show,Typeable,Data)


-- | 
data MachineType = TeVatron 
                 | LHC7 Detector
                 | LHC8 Detector
                 | LHC10 Detector
                 | LHC14 Detector 
                 | Parton Double Detector
                 | PolParton Double InitPolarization Detector
                 deriving (Show,Typeable,Data)

-- | 
data RGRunType = Fixed | Auto 
               deriving (Show,Typeable,Data) 

-- | 
data CutType = NoCut | DefCut | KCut 
             deriving (Show,Typeable,Data)

-- | 
data MatchType = NoMatch | MLM
               deriving (Show,Typeable,Data)

-- | 
data PYTHIAType = NoPYTHIA | RunPYTHIA
                deriving (Show,Typeable,Data)


-- | jet algorithm implemented in PGS (Cone, K_T , anti-K_T algorithm) 
data PGSJetAlgorithm = Cone Double | KTJet Double | AntiKTJet Double
                       deriving (Show, Typeable, Data)

-- | 
data PGSTau = NoTau | WithTau
              deriving (Show,Typeable,Data)

-- | 
type PGSJetAlgoNTau = (PGSJetAlgorithm,PGSTau) 


-- | 
data PGSType = NoPGS | RunPGS PGSJetAlgoNTau 
             deriving (Show,Typeable,Data)


-- | 
data HEPFileType = NoUploadHEP | UploadHEP
                   deriving (Show, Typeable, Data)



