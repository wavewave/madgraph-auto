{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.Machine 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module HEP.Automation.MadGraph.Card where

import           Control.Applicative
import           Data.Hashable 
import           Text.StringTemplate
import           Text.StringTemplate.Helpers
import           System.FilePath ((</>))
--
import HEP.Automation.MadGraph.Type




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
runCardSetup :: FilePath 
             -> MachineType 
             -> CutType 
             -> MatchType 
             -> RGRunType 
             -> Double 
             -> Int        -- ^ number of events 
             -> HashSalt  
             -> Int        -- ^ set number
             -> IO String 
runCardSetup tpath machine ctype mtype rgtype scale numevt hsalt setnum = do 
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
      iseed = case unHashSalt hsalt of
                Nothing -> show setnum  
                Just hs -> show (hashWithSalt hs setnum `mod` 1000 + 1001)

  putStrLn "======================================"
  putStrLn "======================================"
  putStrLn "======================================"
  putStrLn iseed 
  putStrLn "======================================"
  putStrLn "======================================"
  putStrLn "======================================"



  templates <- directoryGroup tpath 
  return $ (renderTemplateGroup
              templates
              [ ("numevt"       , show numevtfinal ) 
              , ("iseed"        , iseed )
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
      RunPYTHIA8 -> return Nothing 
      RunPYTHIA -> do str <- readFile (tpath </> pythiaCardMatch NoMatch)
                      return (Just (str++"\n\n\n"))

-- | 
                      
pgsCardSetup :: FilePath          -- ^ template path
             -> MachineType       -- ^ machine type
             -> PGSType           -- ^ pgs work type
             -> IO (Maybe String) -- ^ pgs card string
pgsCardSetup tpath machine NoPGS = return Nothing 
pgsCardSetup tpath machine (RunPGS (jetalgo,tau)) = do 
    tmplstr <- (++ "\n\n\n") <$> readFile (tpath </> pgsCardMachine machine) 
    let addnotau = case tau of 
                   NoTau -> "notau"
                   WithTau -> ""  
    let str = case jetalgo of 
          Cone conesize  -> render1 [ ("jetalgo", "cone"++addnotau)
                                    , ("conesize", show conesize) ] tmplstr
          KTJet conesize -> render1 [ ("jetalgo", "ktjet"++addnotau) 
                                    , ("conesize", show conesize) ] tmplstr
          AntiKTJet conesize -> render1 [ ("jetalgo", "antikt"++addnotau) 
                                    , ("conesize", show conesize) ] tmplstr
    return (Just str)








