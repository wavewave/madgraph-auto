module Main where

import System.IO
import System.Process
import System.Directory

import Control.Monad

import Text.StringTemplate
import Text.StringTemplate.Helpers

modelWorkdirTaskname = [ ("fvwp200_MG","20110110Wp200","WP200SingleWpLHC7FixRG200")
                       , ("fvwp300_MG","20110110Wp300","WP300SingleWpLHC7FixRG200")
                       , ("fvwp400_MG","20110110Wp400","WP400SingleWpLHC7FixRG200")
                       , ("fvwp600_MG","20110110Wp600","WP600SingleWpLHC7FixRG200") ]


createWorkDirAndGenerateEvents (model,workdir,taskname) = do
  putStrLn $ "now entering task = " ++ taskname
  
  putStrLn $ "set up a working directory" 

  let mybase = "/home/wavewave/nfs/workspace/ttbar/madgraph_auto/" 
      mytaskbase = mybase ++ "20110110_SingleWpGen/"
      newproccardname = "proc_card.dat." ++ taskname 
      dir_mg5 =  "/home/wavewave/nfs/montecarlo/MG_ME_V4.4.44/MadGraph5_v0_6_1/"
      dir_mg4 =  "/home/wavewave/nfs/montecarlo/MG_ME_V4.4.44/"

  
  setCurrentDirectory mytaskbase
  
  templates <- directoryGroup "template"
  let newproccardmg5 = renderTemplateGroup 
                         templates 
                         [ ("model",model)
                         , ("workdirname",workdir) ] 
                         "proc_card_mg5.dat.forSingleWpGen.20110110"
  
  writeFile (mytaskbase ++ "working/" ++ newproccardname ) newproccardmg5
  
  setCurrentDirectory dir_mg5 
  
  readProcess ("bin/mg5") [mytaskbase ++ "working/" ++ newproccardname] ""

  putStrLn "moving directory"
  
  renameDirectory (dir_mg5 ++ workdir) (dir_mg4 ++ workdir) 


  putStrLn "generating LHC7 events"

  forM ["run_card.dat", "pythia_card.dat", "pgs_card.dat" ] $
    \x -> copyFile (mytaskbase ++ "template/" ++ x) (dir_mg4 ++ workdir ++ "/Cards/" ++ x)

  setCurrentDirectory (dir_mg4 ++ workdir) 
  
  readProcess ("bin/generate_events") ["2", "6", taskname] ""

  return ()

main :: IO () 
main = do 
  mapM_ createWorkDirAndGenerateEvents modelWorkdirTaskname 
  