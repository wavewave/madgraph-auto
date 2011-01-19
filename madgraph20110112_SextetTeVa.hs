{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import System.Process
import System.Directory

import Control.Monad

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Text.Printf

modelWorkdirTaskname = [ ("20110112SixTeV600","Six600ttbarTeVaFixRG200",600)
                       , ("20110112SixTeV800","Six800ttbarTeVaFixRG200",800)
                       , ("20110112SixTeV1000","Six1000ttbarTevaFixRG200",1000)
                       ]
                       
ysfrom mphi = mphi / 257.0  + 1.28            

createWorkDirAndGenerateEvents :: (String, String,Double) -> IO ()
createWorkDirAndGenerateEvents (workdir,taskname,mphi) = do
  let model = "sextets_fv" 
        
  putStrLn $ "now entering task = " ++ taskname
  
  putStrLn $ "set up a working directory" 

  let mybase = "/home/wavewave/ttbar/madgraph_auto/" 
      mytaskbase = mybase ++ "20110112_SextetTeVa/"
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
  let newparamcard = renderTemplateGroup 
                         templates 
                         [ ("mphi",(printf "%e" mphi :: String) )
                         , ("ys",( printf "%e" (ysfrom mphi) :: String)  ) ] 
                         "param_card.dat"

  
  
  writeFile (mytaskbase ++ "working/" ++ newproccardname ) newproccardmg5
  writeFile (mytaskbase ++ "working/" ++ newparamcardname ) newparamcard
  

  setCurrentDirectory dir_mg5 
  
  readProcess ("bin/mg5") [mytaskbase ++ "working/" ++ newproccardname] ""

  putStrLn "moving directory"
  
  renameDirectory (dir_mg5 ++ workdir) (dir_work ++ workdir) 


  putStrLn "generating TeVatron events"

  forM ["run_card.dat", "pythia_card.dat", "pgs_card.dat" ] $
    \x -> copyFile (mytaskbase ++ "template/" ++ x) (dir_work ++ workdir ++ "/Cards/" ++ x)

  copyFile (mytaskbase ++ "working/" ++ newparamcardname ) (dir_work ++ workdir ++ "/Cards/param_card.dat" )


  setCurrentDirectory (dir_work ++ workdir) 
  
  readProcess ("bin/generate_events") ["2", "6", taskname] ""

  return ()

main :: IO () 
main = do 
  mapM_ createWorkDirAndGenerateEvents modelWorkdirTaskname 
  