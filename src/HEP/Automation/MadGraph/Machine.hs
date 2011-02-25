module HEP.Automation.MadGraph.Machine where

import Text.StringTemplate
import Text.StringTemplate.Helpers


data MachineType = TeVatron | LHC7 | LHC14 | TeVatronParton
                 deriving Show

data RGRunType = Fixed | Auto 

data CutType = NoCut | DefCut | KCut 

data MatchType = NoMatch | MLM

data PYTHIAType = NoPYTHIA | RunPYTHIA

data PGSType = NoPGS | RunPGS


runCard4CutMatch :: CutType -> MatchType -> String
runCard4CutMatch NoCut  NoMatch = "run_card_NoCut_NoMatch.dat"
runCard4CutMatch DefCut MLM     = "run_card_DefCut_MLM.dat"
runCard4CutMatch KCut   MLM     = "run_card_KCut_MLM.dat"
runCard4CutMatch _ _ = error "cut mlm does not match"


pythiaCardMatch :: MatchType -> String
pythiaCardMatch NoMatch = "pythia_card_default.dat"
pythiaCardMatch MLM     = "pythia_card_MLM.dat"

pgsCardMachine :: MachineType -> String 
pgsCardMachine TeVatron = "pgs_card_TEV.dat"
pgsCardMachine LHC7     = "pgs_card_LHC.dat"
pgsCardMachine LHC14    = "pgs_card_LHC.dat"

runCardSetup :: FilePath -> MachineType -> CutType -> MatchType -> RGRunType -> Double -> Int -> IO String 
runCardSetup tpath machine ctype mtype rgtype scale numevt = do 
  let (beamtyp1,beamtyp2,beamenergy) = case machine of 
        TeVatron -> ("1","-1","980")
        LHC7     -> ("1","1","3500")
        LHC14    -> ("1","1","7000")
        TeVatronParton -> ("0","0","980")
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
    MLM -> do str <- readFile (tpath ++ pythiaCardMatch MLM)
              return (Just str)
    NoMatch -> case ptype of
      NoPYTHIA -> return Nothing 
      RunPYTHIA -> do str <- readFile (tpath ++ pythiaCardMatch NoMatch)
                      return (Just (str++"\n\n\n"))
                      
pgsCardSetup :: FilePath -> MachineType -> PGSType -> IO (Maybe String) 
pgsCardSetup tpath machine pgstype = do 
  case pgstype of 
    NoPGS -> return Nothing
    RunPGS -> do str <- readFile (tpath ++ pgsCardMachine machine)
                 return (Just (str++"\n\n\n"))
