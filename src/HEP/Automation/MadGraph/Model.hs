{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.Model 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Common model interface 
--
-----------------------------------------------------------------------------

module HEP.Automation.MadGraph.Model where

import Data.Typeable
import Data.Data
--
import HEP.Automation.MadGraph.Type

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

makeProcessFile :: Model a => a -> MGProcess -> String -> String
makeProcessFile model (MGProc predef processes) dirname = 
  let importline = case madgraphVersion model of
        MadGraph4 -> "import model_v4 " ++ modelName model
        MadGraph5 -> "import model " ++ modelName model
      predefstr = unlines predef
      processstr = case processes of 
                     [] -> error "makeProcessFile : no process "
                     x:xs -> let p' = ("generate " ++ x)  
                                      : map ("add process " ++) xs
                             in unlines p' 
  in importline ++ "\n" ++ predefstr ++ "\n" ++ processstr ++ "\n" ++ "output " ++ dirname ++ "\n\n" 

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


{- 
modelParamTc :: TyCon
modelParamTc = mkTyCon3 "HEP.Automation.MadGraph.Model.ModelParam"


dummyModelTr :: TypeRep
dummyModelTr = mkTyConApp (mkTyCon3 "HEP.Automation.MadGraph.Model.DummyModel") []

instance Typeable (ModelParam DummyModel) where
  -- typeOf _ = mkTyConApp modelParamTc [dummyModelTr]
  typeRep _ = mkTyConApp modelParamTc [dummyModelTr]
-}

deriving instance Typeable ModelParam

deriving instance Data (ModelParam DummyModel)

