module Main where

import System.IO
import System.Process
import System.Directory


import Text.StringTemplate
import Text.StringTemplate.Helpers

tasklist = ["500","1000","1500","2000","2500","3000","3500"
           ,"4000","4500","5000","5500","6000","6500","7000"
           ,"7500","8000","8500","9000","9500","10000"]

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
  let tasklist = [  1,  2,  3,  4,  5,  6,  7,  8,  9, 10
                 , 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 ]
      tasknamelist = map (\x-> "SM_ttbar_0or1jet_MLM_FixedRG200_LHC7_set" ++ (show x) ) tasklist           
  mapM_ singletask tasknamelist
  