{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, DeriveDataTypeable, StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.Model 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module HEP.Automation.MadGraph.Model where

import Data.Typeable
import Data.Data


class (Show a, Typeable a, Data a, Show (ModelParam a),Typeable (ModelParam a),Data (ModelParam a)) => Model a where
  data ModelParam a :: * 
  briefShow       :: a -> String 
  madgraphVersion :: a -> MadGraphVersion
  modelName       :: a -> String 
  modelFromString :: String -> Maybe a
  paramCard4Model :: a -> String 
  paramCardSetup  :: FilePath -> a -> ModelParam a -> IO String 
  briefParamShow  :: ModelParam a -> String 
  interpreteParam :: String -> ModelParam a

data MadGraphVersion = MadGraph4 | MadGraph5
                  deriving (Show,Typeable,Data)

makeProcessFile :: Model a => a -> String -> String -> String
makeProcessFile model process dirname = 
  let importline = case madgraphVersion model of
        MadGraph4 -> "import model_v4 " ++ modelName model
        MadGraph5 -> "import model " ++ modelName model
  in importline ++ "\n" ++ process ++ "\n" ++ "output " ++ dirname ++ "\n\n" 

data DummyModel = DummyModel 
                deriving (Show,Typeable,Data) 
                        
instance Model DummyModel where
  data ModelParam DummyModel = DummyParam deriving Show
  briefShow _ = "" 
  madgraphVersion _ = MadGraph4
  modelName _ = "DummyModel" 
  modelFromString str = case str of 
                          "DummyModel" -> Just DummyModel 
                          _ -> Nothing 
  paramCard4Model _ = "" 
  paramCardSetup _ _ _ = return "" 
  briefParamShow _ = "" 
  interpreteParam _ = DummyParam

modelParamTc :: TyCon
modelParamTc = mkTyCon "HEP.Automation.MadGraph.Model.ModelParam"


dummyModelTr :: TypeRep
dummyModelTr = mkTyConApp (mkTyCon "HEP.Automation.MadGraph.Model.DummyModel") []

instance Typeable (ModelParam DummyModel) where
  typeOf _ = mkTyConApp modelParamTc [dummyModelTr]

deriving instance Data (ModelParam DummyModel)
