{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}

module HEP.Automation.MadGraph.Model where

import Text.Printf

import Text.StringTemplate
import Text.StringTemplate.Helpers

class (Show a, Show (ModelParam a)) => Model a where
  data ModelParam a :: * 
  briefShow       :: a -> String 
  modelName       :: a -> String 
  paramCard4Model :: a -> String 
  paramCardSetup  :: FilePath -> a -> ModelParam a -> IO String 
  briefParamShow  :: ModelParam a -> String 

data MadGraphVersion = MadGraph4 | MadGraph5
                  deriving Show


makeProcessFile :: Model a => a  -> MadGraphVersion -> String -> String -> String
makeProcessFile model mgver process dirname = 
  let importline = case mgver of
        MadGraph4 -> "import model_v4 " ++ modelName model
        MadGraph5 -> "import model " ++ modelName model
  in importline ++ "\n" ++ process ++ "\n" ++ "output " ++ dirname ++ "\n\n" 

