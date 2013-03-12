{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.Machine 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module HEP.Automation.MadGraph.Machine where

import           Control.Applicative
import           Data.Typeable
import           Data.Data
import           Text.StringTemplate
import           Text.StringTemplate.Helpers
import           System.FilePath ((</>))

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

-- | 

data PGSType = NoPGS | RunPGS 
             deriving (Show,Typeable,Data)

-- | 
data PGSJetAlgorithm = Cone Double | KTJet Double | AntiKTJet Double
                       deriving (Show, Typeable, Data)

-- | 
data PGSTau = NoTau | WithTau
              deriving (Show,Typeable,Data)

-- | 
type PGSJetAlgoNTau = (PGSJetAlgorithm,PGSTau) 


-- | 

data HEPFileType = NoUploadHEP | UploadHEP
                   deriving (Show, Typeable, Data)

-- | 

runCard4CutMatch :: CutType -> MatchType -> String
runCard4CutMatch NoCut  NoMatch = "run_card_NoCut_NoMatch.dat"
runCard4CutMatch DefCut NoMatch = "run_card_DefCut_NoMatch.dat"
runCard4CutMatch DefCut MLM     = "run_card_DefCut_MLM.dat"
runCard4CutMatch KCut   MLM     = "run_card_KCut_MLM.dat"
runCard4CutMatch _ _ = error "cut mlm does not match"

-- | 

pythiaCardMatch :: MatchType -> String
pythiaCardMatch NoMatch = "pythia_card_default.dat"
pythiaCardMatch MLM     = "pythia_card_MLM.dat"

-- | 

pgsCardMachine :: MachineType -> String 
pgsCardMachine TeVatron = "pgs_card_TEV.dat.st"
pgsCardMachine (LHC7 LHC)    = "pgs_card_LHC.dat.st"
pgsCardMachine (LHC7 ATLAS)  = "pgs_card_ATLAS.dat.st"
pgsCardMachine (LHC7 CMS)    = "pgs_card_CMS.dat.st"
pgsCardMachine (LHC7 _)      = undefined
pgsCardMachine (LHC8 LHC)    = "pgs_card_LHC.dat.st"
pgsCardMachine (LHC8 ATLAS)  = "pgs_card_ATLAS.dat.st"
pgsCardMachine (LHC8 CMS)    = "pgs_card_CMS.dat.st"
pgsCardMachine (LHC8 _)      = undefined
pgsCardMachine (LHC10 LHC)   = "pgs_card_LHC.dat.st"
pgsCardMachine (LHC10 ATLAS) = "pgs_card_ATLAS.dat.st"
pgsCardMachine (LHC10 CMS)   = "pgs_card_CMS.dat.st"
pgsCardMachine (LHC10 _)     = undefined
pgsCardMachine (LHC14 LHC)   = "pgs_card_LHC.dat.st"
pgsCardMachine (LHC14 ATLAS) = "pgs_card_ATLAS.dat.st"
pgsCardMachine (LHC14 CMS)   = "pgs_card_CMS.dat.st"
pgsCardMachine (LHC14 _)     = undefined
pgsCardMachine (Parton _ LHC) = "pgs_card_LHC.dat.st"
pgsCardMachine (Parton _ Tevatron) = "pgs_card_TEV.dat.st"
pgsCardMachine (Parton _ ATLAS) = "pgs_card_ATLAS.dat.st"
pgsCardMachine (Parton _ CMS) = "pgs_card_CMS.dat.st"
pgsCardMachine (PolParton _ _ LHC) = "pgs_card_LHC.dat.st"
pgsCardMachine (PolParton _ _ Tevatron) = "pgs_card_TEV.dat.st"
pgsCardMachine (PolParton _ _ ATLAS) = "pgs_card_ATLAS.dat.st"
pgsCardMachine (PolParton _ _ CMS) = "pgs_card_CMS.dat.st"

-- | 

runCardSetup :: FilePath -> MachineType -> CutType -> MatchType -> RGRunType -> Double -> Int -> Int -> IO String 
runCardSetup tpath machine ctype mtype rgtype scale numevt setnum = do 
  let (beamtyp1,beamtyp2,beamenergy,beampol1,beampol2) = case machine of 
        TeVatron -> ("1","-1","980","0","0")
        LHC7 _    -> ("1","1","3500","0","0")
        LHC8 _    -> ("1","1","4000","0","0")
        LHC10 _   -> ("1","1","5000","0","0")
        LHC14 _   -> ("1","1","7000","0","0")
        Parton be _ -> ("0","0", show be,"0","0")
        PolParton be ipol _ -> ( "0","0", show be
                               , (show . rhpol_percent . particle1pol) ipol
                               , (show . rhpol_percent . particle2pol) ipol ) 
      isFixedRG = case rgtype of 
        Fixed -> "T"
        Auto  -> "F"
      numevtfinal = if numevt > 100000 then 100000 else numevt 
      
  templates <- directoryGroup tpath 
  return $ (renderTemplateGroup
              templates
              [ ("numevt"       , show numevtfinal ) 
              , ("iseed"        , show setnum )
	      , ("beamTypeOne"  , beamtyp1   )
              , ("beamTypeTwo"  , beamtyp2   )
              , ("beamEnergy"   , beamenergy )
              , ("beamPolOne"   , beampol1)
              , ("beamPolTwo"   , beampol2)
              , ("isFixedRG"    , isFixedRG  )
              , ("rgScale"      , show scale ) 
              , ("facScale"     , show scale ) ]
              (runCard4CutMatch ctype mtype)  ) ++ "\n\n\n"

-- | 

pythiaCardSetup :: FilePath -> MatchType -> PYTHIAType -> IO (Maybe String)
pythiaCardSetup tpath mtype ptype = do  
  case mtype of 
    MLM -> do str <- readFile (tpath </> pythiaCardMatch MLM)
              return (Just str)
    NoMatch -> case ptype of
      NoPYTHIA -> return Nothing 
      RunPYTHIA -> do str <- readFile (tpath </> pythiaCardMatch NoMatch)
                      return (Just (str++"\n\n\n"))

-- | 
                      
pgsCardSetup :: FilePath          -- ^ template path
             -> MachineType       -- ^ machine type
             -> PGSType           -- ^ pgs work type
             -> PGSJetAlgoNTau    -- ^ pgs algorithm and whether identify taus
             -> IO (Maybe String) -- ^ pgs card string
pgsCardSetup tpath machine pgstype (jetalgo,tau) = do 
  pgscardstr <- case pgstype of 
                  NoPGS -> return Nothing
                  RunPGS -> do str <- readFile (tpath </> pgsCardMachine machine)
                               return (Just (str++"\n\n\n"))
{-                   RunPGSNoTau -> do str <- readFile (tpath </> pgsCardMachine machine)
                                    return (Just (str++"\n\n\n")) -}
  let addnotau = case tau of 
                   NoTau -> "notau"
                   WithTau -> ""  
  let mstr = case jetalgo of 
        Cone conesize  -> render1 [ ("jetalgo", "cone"++addnotau)
                                  , ("conesize", show conesize) ] <$> pgscardstr
        KTJet conesize -> render1 [ ("jetalgo", "ktjet"++addnotau) 
                                  , ("conesize", show conesize) ] <$> pgscardstr
        AntiKTJet conesize -> render1 [ ("jetalgo", "antikt"++addnotau) 
                                  , ("conesize", show conesize) ] <$> pgscardstr
  return mstr 







