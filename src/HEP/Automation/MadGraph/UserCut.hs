{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.UserCut 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module HEP.Automation.MadGraph.UserCut where

import Data.Typeable
import Data.Data

import Text.StringTemplate
import Text.StringTemplate.Helpers

data UserCutSet = NoUserCutDef | UserCutDef UserCut
                deriving (Show, Typeable, Data)

data UserCut = UserCut {
    uc_metcut    :: Double 
  , uc_etacutlep :: Double 
  , uc_etcutlep  :: Double
  , uc_etacutjet :: Double
  , uc_etcutjet  :: Double  
} deriving (Show, Typeable, Data)

prettyprintUserCut :: UserCut -> String 
prettyprintUserCut uc = 
  let header =  "\n\n\n" 
             ++ "-------------------------------\n" 
             ++ "---       USER CUT         ----\n"
             ++ "-------------------------------\n"
             ++ "\n\n\n"
      footer =  "\n\n\n" 
             ++ "-------------------------------\n"
             ++ "---      END USER CUT      ----\n"
             ++ "-------------------------------\n"
             ++ "\n\n\n" 
      content=  " Missing ET cut = " ++ show (uc_metcut uc) ++ " GeV \n"
             ++ " Lepton eta cut = " ++ show (uc_etacutlep uc) ++ " \n"
             ++ " Lepton  ET cut = " ++ show (uc_etcutlep uc) ++ " GeV \n"
             ++ " Jet    eta cut = " ++ show (uc_etacutjet uc) ++ " \n"
             ++ " Jet     ET cut = " ++ show (uc_etcutjet uc) ++ " GeV \n"
  in  header ++ content ++ footer

hep2lheSetup :: FilePath -> UserCut -> IO String 
hep2lheSetup fp uc = do 
  templates <- directoryGroup fp 
  return $ (renderTemplateGroup 
              templates
              [ ("metcut"      , show (uc_metcut    uc))
	      , ("etacutlep"   , show (uc_etacutlep uc))
              , ("etcutlep"    , show (uc_etcutlep  uc))
              , ("etacutjet"   , show (uc_etacutjet uc))
              , ("etcutjet"    , show (uc_etcutjet  uc)) ] 
              "hep2lhe.f") ++ "\n\n\n" 

