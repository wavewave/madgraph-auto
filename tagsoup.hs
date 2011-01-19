{-# LANGUAGE PackageImports, ScopedTypeVariables #-}

module Main where

import System.IO
import System.Directory


import HROOT

import Data.List 
import Data.List.Split
import Data.Maybe
import Data.Ord
import Text.HTML.TagSoup

import "mtl" Control.Monad.State

eps = 1e-3

fst3 (x,y,z) = x
snd3 (x,y,z) = y
trd3 (x,y,z) = z

isTagOpenTr (TagOpen "TR" _) = True 
isTagOpenTr (TagOpen "tr" _) = True 
isTagOpenTr _ = False

isTagCloseTr (TagClose "TR") = True
isTagCloseTr (TagClose "tr") = True
isTagCloseTr _ = False

isStarter (TagText  "   fermi       ") = True
isStarter _ = False

isEnder = isTagCloseTr


firstChunk :: State [Tag String] [Tag String]
firstChunk = do currlst <- get
                let (_,lst1) = break isStarter currlst
                 in  if null lst1
                       then do put []
                               return []
                       else let (lst2,lst3) = break isEnder $ tail lst1
                            in  if null lst3
                                then do put []
                                        return lst2
                                else do put $ tail lst3
                                        return lst2

matchpattern [_,_,_,TagText energystring,_,_,_,_,_,_,_,_,_,TagText xsecstr,_,_,_,_,_] = Just (energystring, xsecstr) 
matchpattern _ = Nothing

onlynumeric = filter (\c-> c >= '0' && c <= '9' )  


parseMass :: String -> Double
parseMass = read . (!!1) . (filter (not.null)) . splitOneOf (['A'..'Z'] ++ ['a'..'z']  ) . (filter (/= ' '))

parseCoup :: String -> Double 
parseCoup = read . (!!2) . (filter (not.null)) . splitOneOf (['A'..'Z'] ++ ['a'..'z']  ) . (filter (/= ' '))



parseMassCoupXsec :: (String,String) -> (Double,Double,Double)
parseMassCoupXsec (x,y) = (parseMass x, parseCoup x, read $ '0' : (f y) )
  where f = filter (/= ' ') 


oneFileProcess  :: TCanvas -> FilePath -> Color -> Bool -> IO () 
oneFileProcess canvas fp color isFirst = do 
  h <- openFile fp ReadMode
  str <- hGetContents h 
  let listparsed  = parseTags str 
      chunkparsed = takeWhile (not.null) 
                    $ evalState (sequence (repeat firstChunk)) listparsed
      matched = map matchpattern chunkparsed 
      result2 = sortBy (comparing fst3) . filter (\x-> snd3 x < 2.0+eps && snd3 x > 2.0-eps)  $ map (parseMassCoupXsec . fromJust) matched
      result4 = sortBy (comparing fst3) . filter (\x-> snd3 x < 4.0+eps && snd3 x > 4.0-eps)  $ map (parseMassCoupXsec . fromJust) matched
      result6 = sortBy (comparing fst3) . filter (\x-> snd3 x < 6.0+eps && snd3 x > 6.0-eps)  $ map (parseMassCoupXsec . fromJust) matched

  
  putStrLn "result2"
  putStrLn $ intercalate "\n" $ map show result2 
  putStrLn "result4"
  putStrLn $ intercalate "\n" $ map show result4 
  putStrLn "result6"
  putStrLn $ intercalate "\n" $ map show result6 
  
  [graph2,graph4,graph6] <- mapM drawGraph [result2,result4,result6] 
  
  mapM_ (\x->setLineColor x color) [graph2,graph4,graph6]
  
  if isFirst 
    then mapM_ (\x->draw x "AL") [graph2]
    else mapM_ (\x->draw x "same") [graph2] 
                  
  mapM_ (\x->draw x "same") [graph4,graph6]

  hClose h
  

drawGraph result = do
  let x = map fst3 result 
      y = map trd3 result   
      n = length result
  
  graph <- newTGraph n x y
  
  setName graph "ttbar SM #sqrt{s} (TeV) vs cross section (pb) " 
  setTitle graph "ttbar SM #sqrt{s} (TeV) vs cross section (pb) : triplet model" 

  return graph


main :: IO () 
main = do 
  canvas <- newTCanvas "ttbar SM #sqrt{s} (TeV) vs cross section (pb) " "ttbar SM #sqrt{s} (TeV) vs cross section (pb)" 640 480
  setBorderMode canvas kBorderNo 
  setFillColor canvas kWhite
  
  oneFileProcess canvas "crossx_triplet.html" kRed True
  
  oneFileProcess canvas "crossx_sextet.html" kBlue False

  saveAs canvas "canvastest.pdf" ""
  

resultparse :: (String,String) -> (Double,Double)
resultparse (x,y) = (read $ onlynumeric x, read $ '0' : (f y) )
  where f = filter (/= ' ') 
         