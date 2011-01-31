{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import System.Process
import System.Directory

import Control.Monad

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Text.Printf

modelWorkdirTaskname = [ ("fvwp200_MG","20110117Wp2","Wp200_ttbar01jet_MLM_TeVa_KCut",i) | i <- [1..10] ] 
                      
createWorkDir :: (String,String,String) -> IO ()
createWorkDir (model,workdir,taskname',num) = do
  let taskname = taskname' ++ "set" ++ show num
  putStrLn $ "now entering task = " ++ taskname
  
  putStrLn $ "set up a working directory" 

  let mybase = "/home/wavewave/ttbar/madgraph_auto/" 
      mytaskbase = mybase ++ "20110117_WpTTBarMLMTeVKCut/"
      newproccardname = "proc_card_mg5.dat." ++ taskname 
      newparamcardname = "param_card.dat." ++ taskname
      dir_mg5 =  "/home/wavewave/nfs/montecarlo/MG_ME_V4.4.44/MadGraph5_v0_6_1/"
      dir_work =  "/home/wavewave/nfs/workspace/ttbar/mc/"

  
  setCurrentDirectory mytaskbase
  
  templates <- directoryGroup "template"
  let newproccardmg5 = renderTemplateGroup 
                         templates 
                         [ ("model",model)
                         , ("workdirname",workdir) ] 
                         "proc_card_mg5.dat"
  
  
  writeFile (mytaskbase ++ "working/" ++ newproccardname ) newproccardmg5
 

  setCurrentDirectory dir_mg5 
  
  readProcess ("bin/mg5") [mytaskbase ++ "working/" ++ newproccardname] ""

  putStrLn "moving directory"
  
  renameDirectory (dir_mg5 ++ workdir) (dir_work ++ workdir) 


  putStrLn "generating TeVatron events"

  forM ["run_card.dat", "pythia_card.dat", "pgs_card.dat" ] $
    \x -> copyFile (mytaskbase ++ "template/" ++ x) (dir_work ++ workdir ++ "/Cards/" ++ x)

  setCurrentDirectory (dir_work ++ workdir) 
  
  readProcess ("bin/generate_events") ["2", "6", taskname] ""

  return ()

main :: IO () 
main = do 
  mapM_ createWorkDirAndGenerateEvents modelWorkdirTaskname 
  