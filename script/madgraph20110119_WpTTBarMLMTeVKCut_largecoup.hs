module Main where

import Control.Monad
import System.IO
import System.Process
import System.Directory
import Text.Printf

import Text.StringTemplate
import Text.StringTemplate.Helpers

import CommonWork


taskinfo = TS { 
    model = "fvwp200_MG"
  , basedir = "/home/wavewave/ttbar/madgraph_auto/" 
  , templatename = "20110119_WpTTBarMLMTeVKCut_largecoup/"
  , dir_mg5 = "/home/wavewave/nfs/montecarlo/MG_ME_V4.4.44/MadGraph5_v0_6_1/"
  , dir_work = "/home/wavewave/nfs/workspace/ttbar/mc/"
  , tasknamebase = "WpTTBarMLMTeVKCutCoup"  
  , workdirname = "20110119WpC"  
  }
  
mtop = 174.3           

gamma :: Double -> Double -> Double            
gamma mass coup = 
  let r = mtop^2 / mass^2 
  in  coup^2 / (16.0 * pi) * mass * ( 1.0 - 1.5 * r + 0.5 * r^3 ) 
           
paramset :: TaskSpecifier -> Double -> Double -> Int -> IO String 
paramset param mass coup set = do 
  let taskname = tasknamebase param ++ "Mass" ++ show mass 
                                    ++ "Coup" ++ show coup 
                                    ++ "set"  ++ show set 
  let mytemplatebase = basedir param ++ templatename param
      newparamcardname = "param_card.dat." ++ taskname

  setCurrentDirectory mytemplatebase
  templates <- directoryGroup "template"
  let newparamcard = renderTemplateGroup 
                       templates 
                       [ ("mass",(printf "%e" mass :: String) )
                       , ("coup",( printf "%e" coup :: String)  ) 
                       , ("gamma", (printf "%e" (gamma mass coup) :: String))] 
                       "param_card.dat"
  writeFile (mytemplatebase ++ "working/" ++ newparamcardname ) newparamcard
  copyFile (mytemplatebase ++ "working/" ++ newparamcardname ) (dir_work param ++ workdirname param ++ "/Cards/param_card.dat" )
  
  return taskname

masslist = [200] -- [200,300,400,600] 
couplist = [1e-6] -- [4.0,1e-6]   
setlist :: [Int]
setlist  = [11..100]
    
totallist = [(x,y,z) | x<-masslist, y<-couplist, z<-setlist ]

worker (x,y,z) = do let action = paramset taskinfo x y z
                    generateEvents taskinfo action
main :: IO () 
main = do 
--  createWorkDir taskinfo 
  mapM_ worker totallist
