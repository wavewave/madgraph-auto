module Main where

import System.IO
import System.Process
import System.Directory


import Text.StringTemplate
import Text.StringTemplate.Helpers

singletask taskname = do
  putStrLn $ "now entering task = " ++ taskname
  
  let dirname =  "/home/wavewave/nfs/workspace/ttbar/mc/20110117Wp/"
      
  --   writeFile (dirname ++ "Cards/run_card.dat") newruncard
  
  setCurrentDirectory dirname 
  readProcess (dirname ++ "bin/generate_events") ["2", "2", taskname] ""

  return ()

main :: IO () 
main = do 
  let tasklist = [ 11..100 ] 
      tasknamelist = map (\x-> "Wp200_ttbar01jet_MLM_TeVa_KCut_FixedRG200_set" ++ (show x) ) tasklist           
  mapM_ singletask tasknamelist
  