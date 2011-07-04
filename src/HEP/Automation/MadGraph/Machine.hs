{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.MadGraph.Machine where

import Control.Applicative

import Data.Typeable
import Data.Data

import Text.StringTemplate
import Text.StringTemplate.Helpers

import System.FilePath ((</>))

data Detector = Tevatron | LHC | CMS | ATLAS
              deriving (Show,Typeable,Data)

data MachineType = TeVatron 
                 | LHC7 
                 | LHC14 
                 | Parton Double Detector
                 deriving (Show,Typeable,Data)

data RGRunType = Fixed | Auto 
               deriving Show 

data CutType = NoCut | DefCut | KCut 
             deriving Show

data MatchType = NoMatch | MLM
               deriving Show

data PYTHIAType = NoPYTHIA | RunPYTHIA
                deriving Show

data PGSType = NoPGS | RunPGS | RunPGSNoTau
             deriving Show

data PGSJetAlgorithm = Cone Double | KTJet Double | AntiKTJet Double
                       deriving (Show, Typeable, Data)

data HEPFileType = NoUploadHEP | UploadHEP
                   deriving (Show, Typeable, Data)

runCard4CutMatch :: CutType -> MatchType -> String
runCard4CutMatch NoCut  NoMatch = "run_card_NoCut_NoMatch.dat"
runCard4CutMatch DefCut NoMatch = "run_card_DefCut_NoMatch.dat"
runCard4CutMatch DefCut MLM     = "run_card_DefCut_MLM.dat"
runCard4CutMatch KCut   MLM     = "run_card_KCut_MLM.dat"
runCard4CutMatch _ _ = error "cut mlm does not match"


pythiaCardMatch :: MatchType -> String
pythiaCardMatch NoMatch = "pythia_card_default.dat"
pythiaCardMatch MLM     = "pythia_card_MLM.dat"

pgsCardMachine :: MachineType -> String 
pgsCardMachine TeVatron = "pgs_card_TEV.dat.st"
pgsCardMachine LHC7     = "pgs_card_LHC.dat.st"
pgsCardMachine LHC14    = "pgs_card_LHC.dat.st"
pgsCardMachine (Parton _ LHC) = "pgs_card_LHC.dat.st"
pgsCardMachine (Parton _ Tevatron) = "pgs_card_TEV.dat.st"
pgsCardMachine (Parton _ ATLAS) = "pgs_card_ATLAS.dat.st"
pgsCardMachine (Parton _ CMS) = "pgs_card_CMS.dat.st"

runCardSetup :: FilePath -> MachineType -> CutType -> MatchType -> RGRunType -> Double -> Int -> IO String 
runCardSetup tpath machine ctype mtype rgtype scale numevt = do 
  let (beamtyp1,beamtyp2,beamenergy) = case machine of 
        TeVatron -> ("1","-1","980")
        LHC7     -> ("1","1","3500")
        LHC14    -> ("1","1","7000")
        Parton be _ -> ("0","0", show be)
      isFixedRG = case rgtype of 
        Fixed -> "T"
        Auto  -> "F"
      numevtfinal = if numevt > 100000 then 100000 else numevt 
      
  templates <- directoryGroup tpath 
  return $ (renderTemplateGroup
              templates
              [ ("numevt"       , show numevtfinal ) 
	      , ("beamTypeOne"  , beamtyp1   )
              , ("beamTypeTwo"  , beamtyp2   )
              , ("beamEnergy" , beamenergy )
              , ("isFixedRG"  , isFixedRG  )
              , ("rgScale"    , show scale ) 
              , ("facScale"   , show scale ) ]
              (runCard4CutMatch ctype mtype)  ) ++ "\n\n\n"


pythiaCardSetup :: FilePath -> MatchType -> PYTHIAType -> IO (Maybe String)
pythiaCardSetup tpath mtype ptype = do  
  case mtype of 
    MLM -> do str <- readFile (tpath </> pythiaCardMatch MLM)
              return (Just str)
    NoMatch -> case ptype of
      NoPYTHIA -> return Nothing 
      RunPYTHIA -> do str <- readFile (tpath </> pythiaCardMatch NoMatch)
                      return (Just (str++"\n\n\n"))
                      
pgsCardSetup :: FilePath -> MachineType -> PGSType -> PGSJetAlgorithm -> IO (Maybe String) 
pgsCardSetup tpath machine pgstype jetalgo = do 
  pgscardstr <- case pgstype of 
                  NoPGS -> return Nothing
                  RunPGS -> do str <- readFile (tpath </> pgsCardMachine machine)
                               return (Just (str++"\n\n\n"))
                  RunPGSNoTau -> do str <- readFile (tpath </> pgsCardMachine machine)
                                    return (Just (str++"\n\n\n"))
  case jetalgo of 
    Cone conesize ->  return $ render1 [ ("jetalgo", "cone")
                                       , ("conesize", show conesize) ] <$> pgscardstr
    KTJet conesize -> return $ render1 [ ("jetalgo", "ktjet") 
                                       , ("conesize", show conesize) ] <$> pgscardstr
    AntiKTJet conesize -> return $ render1 [ ("jetalgo", "antikt") 
                                           , ("conesize", show conesize) ] <$> pgscardstr