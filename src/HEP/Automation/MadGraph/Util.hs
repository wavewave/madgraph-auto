{-# LANGUAGE ScopedTypeVariables #-}

-- This is V3
module HEP.Automation.MadGraph.Util where

import HEP.Automation.MadGraph.Model 
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.SetupType

import qualified Data.ByteString.Char8  as B
import Crypto.Classes
import Data.Digest.Pure.MD5 

import Control.Applicative

makeRunName :: (Model a) => ProcessSetup a -> RunSetup a -> String 
makeRunName psetup rsetup = 
  let mprefix = briefShow (model psetup)  
      masscoup = briefParamShow (param rsetup) 
      machineName = case (machine rsetup) of 
        TeVatron -> "TeVa" 
        LHC7 detector -> "LHC7" ++ show detector
        LHC14 detector -> "LHC14" ++ show detector
        Parton be detector -> "Ptn" ++ (show be) ++ (show detector)
      matchName = case (match rsetup) of 
        MLM -> "MLM"
        NoMatch -> "NoMatch"
      cutName = case (cut rsetup) of 
        NoCut -> "NoCut"
        DefCut -> "DefCut"
        KCut -> "KCut"
      pgsName = case (pgs rsetup) of
        RunPGSNoTau -> "_NoTau"
        _           -> "" 
      jetalgoName = case (jetalgo rsetup) of
        Cone conesize -> "Cone" ++ show conesize
        KTJet conesize -> "KT" ++ show conesize
        AntiKTJet conesize -> "AntiKT" ++ show conesize
  in  mprefix++masscoup++"_"++processBrief psetup
        ++"_"++machineName++"_"++matchName++"_"++cutName++pgsName
        ++"_"++jetalgoName ++"_Set" ++ show (setnum rsetup)  

naming :: (Model a) => WorkSetup a -> String 
naming = makeRunName <$> ws_psetup <*>  ws_rsetup 

md5naming :: (Model a) => WorkSetup a -> String
md5naming ws = let md5str :: MD5Digest = hash' . B.pack . naming $ ws   
               in  "temp" ++ show md5str 
