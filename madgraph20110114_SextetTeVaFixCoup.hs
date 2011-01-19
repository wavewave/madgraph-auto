{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import System.Process
import System.Directory

import Control.Monad

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Text.Printf


workdirname = "20110114SixTeVaFixCoup"

tasknamebase = "SixTTbarTeVaFix200"

modelMass = [170,190..1500] 
  
coupling = 6.0 -- 4.0 --  2.0 -- [ 2.0, 4.0, 6.0 ] 


makeTaskname mass coup = tasknamebase ++ "Mass" ++ show mass ++ "Coup" ++ show coup
  
 
                       
ysfrom mphi = mphi / 257.0  + 1.28            

createWorkDir :: String -> IO ()
createWorkDir workdir = do 
  let model = "sextets_fv" 
  putStrLn $ "set up a working directory" 

  let mybase = "/home/wavewave/ttbar/madgraph_auto/" 
      mytaskbase = mybase ++ "20110114_SextetTeVaFixCoup/"
      newproccardname = "proc_card_mg5.dat"
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
  forM ["run_card.dat" ] $
    \x -> copyFile (mytaskbase ++ "template/" ++ x) (dir_work ++ workdir ++ "/Cards/" ++ x)
  return ()

generateEvents :: (String, String,Double,Double) -> IO ()
generateEvents (workdir,taskname,mphi,coup) = do
  putStrLn $ "now entering task = " ++ taskname
  putStrLn "generating TeVatron events"
  let mybase = "/home/wavewave/ttbar/madgraph_auto/" 
      mytaskbase = mybase ++ "20110114_SextetTeVaFixCoup/"
      newparamcardname = "param_card.dat." ++ taskname
      dir_work =  "/home/wavewave/nfs/workspace/ttbar/mc/"
      
  setCurrentDirectory mytaskbase
  templates <- directoryGroup "template"

  let newparamcard = renderTemplateGroup 
                       templates 
                       [ ("mphi",(printf "%e" mphi :: String) )
                       , ("ys",( printf "%e" coup :: String)  ) ] 
                       "param_card.dat"
  writeFile (mytaskbase ++ "working/" ++ newparamcardname ) newparamcard
  
  copyFile (mytaskbase ++ "working/" ++ newparamcardname ) (dir_work ++ workdir ++ "/Cards/param_card.dat" )
  setCurrentDirectory (dir_work ++ workdir) 
  readProcess ("bin/generate_events") ["2", "4", taskname] ""

  return ()

worker x = let taskname = makeTaskname x coupling
           in  generateEvents (workdirname, taskname, x, coupling)
  
  
main :: IO () 
main = do 
--  createWorkDir workdirname
  mapM_ worker modelMass 
  