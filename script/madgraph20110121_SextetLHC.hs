{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import System.Process
import System.Directory

import Control.Monad

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Text.Printf

import CommonWork

taskinfo = TS { 
    model = "sextets_fv"
  , basedir = "/home/wavewave/ttbar/madgraph_auto/" 
  , templatename = "20110121_SextetGamma/"
  , dir_mg5 = "/home/wavewave/nfs/montecarlo/MG_ME_V4.4.44/MadGraph5_v0_6_1/"
  , dir_work = "/home/wavewave/nfs/workspace/ttbar/mc/"
  , tasknamebase = "SixSingleLHC7DCUT"  
  , workdirname = "20110121_6LHC"  
  }
  
mtop = 174.3           
  
paramset :: TaskSpecifier -> Double -> Double -> Double -> Int -> IO String 
paramset param mass coup gamma set = do 
  let taskname = tasknamebase param ++ "Mass" ++ show mass 
                                    ++ "Coup" ++ show coup 
                                    ++ "Gamma" ++ show gamma
                                    ++ "set"  ++ show set 
  let mytemplatebase = basedir param ++ templatename param
      newparamcardname = "param_card.dat." ++ taskname

  setCurrentDirectory mytemplatebase
  templates <- directoryGroup "template"
  let newparamcard = renderTemplateGroup 
                       templates 
                       [ ("mphi",(printf "%e" mass :: String) )
                       , ("ys",( printf "%e" coup :: String)  ) 
                       , ("gamma", (printf "%e" gamma :: String))] 
                       "param_card.dat"
  writeFile (mytemplatebase ++ "working/" ++ newparamcardname ) newparamcard
  copyFile (mytemplatebase ++ "working/" ++ newparamcardname ) (dir_work param ++ workdirname param ++ "/Cards/param_card.dat" )
  
  return taskname

masslist = [200,300,400,600] 
couplist = [1.0] -- [4.0,1e-6]   
gammalist = [1e-6,10.0]
setlist :: [Int]
setlist  = [1]
    
totallist = [(x,y,z,w) | x<-masslist, y<-couplist, z<-gammalist, w<-setlist ]

worker (x,y,z,w) = do let action = paramset taskinfo x y z w
                      generateEvents taskinfo action
main :: IO () 
main = do 
  createWorkDir taskinfo 
  mapM_ worker totallist


  