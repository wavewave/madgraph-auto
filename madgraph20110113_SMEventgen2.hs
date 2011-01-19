module Main where

import System.IO
import System.Process
import System.Directory


import Text.StringTemplate
import Text.StringTemplate.Helpers

singletask taskname = do
  putStrLn $ "now entering task = " ++ taskname
  
  -- setCurrentDirectory "/home/wavewave/nfs/workspace/ttbar/madgraph_auto" 
  -- templates <- directoryGroup "template"
  -- let newruncard = renderTemplateGroup templates [("beamenergy",energy)] "run_card.dat"
  
  let dirname =  "/home/wavewave/nfs/montecarlo/MG_ME_V4.4.32_IW/sm_ttbar_0or1jet/"
      
  --   writeFile (dirname ++ "Cards/run_card.dat") newruncard
  
  setCurrentDirectory dirname 
  readProcess (dirname ++ "bin/generate_events") ["2", "6", taskname] ""

  return ()

main :: IO () 
main = do 
  let tasklist = [ 21, 22, 23, 24, 25, 26, 27, 28, 29, 30
                 , 31, 32, 33, 34, 35, 36, 37, 38, 39, 40 ]
      tasknamelist = map (\x-> "SM_ttbar_0or1jet_MLM_FixedRG200_LHC7_set" ++ (show x) ) tasklist           
  mapM_ singletask tasknamelist
  