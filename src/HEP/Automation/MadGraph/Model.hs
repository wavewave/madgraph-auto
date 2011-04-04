module HEP.Automation.MadGraph.Model where

import Text.Printf

import Text.StringTemplate
import Text.StringTemplate.Helpers


data Model = SM | Wp | ZpH | Six | Trip | AxiGluon 
           | SixFull | TripFull | WpZpFull
           deriving Show

data ModelVersion = MadGraph4 | MadGraph5
                  deriving Show

mtop :: Double
mtop = 174.3           


data Param = SMParam 
           | WpParam { massWp :: Double, gRWp :: Double } 
           | ZpHParam { massZp :: Double, gRZp :: Double }
           | SixParam { massSix :: Double, gRSix :: Double } 
           | TripParam { massTrip :: Double, gRTrip :: Double  } 
           | AxiGluonParam { massAxiG :: Double, gVq :: Double , gVt :: Double , gAq :: Double , gAt :: Double } 
           | SixFullParam { massSix :: Double, gRSix :: Double, gRSixD :: Double } 
           | TripFullParam { massTrip :: Double, gRTrip :: Double, gRTripD :: Double } 
           | WpZpFullParam { mWp :: Double,
                             mZp :: Double,  
                             gWpdt :: Double, 
                             gWpub :: Double, 
                             gZpbb :: Double, 
                             gZptt :: Double, 
                             gZpuu :: Double, 
                             gZpdd :: Double }
           deriving Show

modelName :: Model -> String
modelName SM = "sm"
modelName Wp = "fvwp200_MG"
modelName ZpH = "zHorizontal_MG"
modelName Six = "sextets_fv"
modelName Trip = "triplets_fv"
modelName AxiGluon = "Axigluon_AV_MG"
modelName SixFull = "sextetsfull3"
modelName TripFull = "tripletsfull3"
modelName WpZpFull = "fvwpzpLight_MG" 


makeProcessFile :: Model -> ModelVersion -> String -> String -> String
makeProcessFile model modelversion process dirname = 
  let importline = case modelversion of
        MadGraph4 -> "import model_v4 " ++ modelName model
        MadGraph5 -> "import model " ++ modelName model
  in importline ++ "\n" ++ process ++ "\n" ++ "output " ++ dirname ++ "\n\n" 


paramCard4Model :: Model -> String
paramCard4Model SM   = "param_card_sm.dat"
paramCard4Model Wp   = "param_card_wP.dat"
paramCard4Model ZpH  = "param_card_zHorizontal.dat" 
paramCard4Model Six  = "param_card_six.dat"
paramCard4Model Trip = "param_card_trip.dat" 
paramCard4Model AxiGluon = "param_card_axigluon.dat"
paramCard4Model SixFull = "param_card_sixfull.dat"
paramCard4Model TripFull = "param_card_tripfull.dat"
paramCard4Model WpZpFull   = "param_card_wpzpfull.dat"

gammaWpZp :: Double -> Double -> Double            
gammaWpZp mass coup = 
  let r = mtop^(2 :: Int)/ mass^(2 :: Int)  
  in  coup^(2 :: Int) / (16.0 * pi) *mass*( 1.0 - 1.5 * r + 0.5 * r^(3 :: Int))


gammaWpZpFullBelowTopMass :: Double -> Double -> Double -> Double -> Double -> Double-> (Double,Double)  
gammaWpZpFullBelowTopMass mWp mZp gWpub gZpuu gZpdd gZpbb = 
  let gammaWp = 2.0*gWpub^(2::Int)*(1/(16.0*pi)) * mWp 
      gammaZp = (gZpuu^(2::Int)+gZpdd^(2::Int)+gZpbb^(2::Int))*(2.0/(16.0*pi))*mZp 
  in  (gammaWp,gammaZp)

gammaAxigluon :: Double -> Double -> Double -> Double -> Double -> Double -> Double
gammaAxigluon  alphas mass gvq gvt gaq gat = 
  alphas / 3.0 * mass * (gvt^(2 :: Int) + gat^(2 :: Int) 
                         + 2.0 * ( gvq^(2 :: Int) + gaq^(2 :: Int) ) )


decayWidthExotic :: Double -> Double -> Double -> Double
decayWidthExotic  y mphi mt = y^(2 :: Int) / (16.0 * pi )
                              * (mphi^(2 :: Int) - mt^(2 :: Int))^(2 :: Int) 
                              / (mphi^(3 :: Int))



paramCardSetup :: FilePath -> Model -> Param -> IO String
paramCardSetup tpath SM SMParam = do  
  str <- readFile (tpath ++ "/" ++ paramCard4Model SM ++ ".st" )  
  return str
paramCardSetup tpath Wp (WpParam m g) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("massWp"       , (printf "%.4e" m :: String))
               , ("gRWpoverSqrtTwo", (printf "%.4e" (g / (sqrt 2.0)) :: String))
               , ("widthWp"      , (printf "%.4e" (gammaWpZp m g) :: String)) ]
               (paramCard4Model Wp)  ) ++ "\n\n\n"
paramCardSetup tpath ZpH (ZpHParam m g) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("masszp"       , (printf "%.4e" m :: String))
               , ("gRoverSqrtTwo"  , (printf "%.4e" (g / (sqrt 2.0)) :: String))
               , ("widthzp"      , (printf "%.4e" (gammaWpZp m g) :: String)) ]
               (paramCard4Model ZpH) ) ++ "\n\n\n"
paramCardSetup tpath Six (SixParam m g) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("massSix" , (printf "%.4e" m :: String))
               , ("gRSix"   , (printf "%.4e" g :: String))
               , ("widthSix", (printf "%.4e" (decayWidthExotic g m mtop) :: String)) ]
               (paramCard4Model Six) ) ++ "\n\n\n"
paramCardSetup tpath Trip (TripParam m g) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("massTrip" , (printf "%.4e" m :: String))
               , ("gRtrip"   , (printf "%.4e" g :: String))
               , ("widthTrip", (printf "%.4e" (decayWidthExotic g m mtop) :: String)) ]
               (paramCard4Model Trip) ) ++ "\n\n\n"
paramCardSetup tpath AxiGluon (AxiGluonParam m gvq gvt gaq gat) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("maxi" , (printf "%.4e" m :: String))
               , ("gvq"  , (printf "%.4e" gvq :: String))
               , ("gvt"  , (printf "%.4e" gvt :: String))
               , ("gaq"  , (printf "%.4e" gaq :: String))
               , ("gat"  , (printf "%.4e" gat :: String))
               , ("waxi", (printf "%.4e" (gammaAxigluon 0.118 m gvq gvt gaq gat) :: String)) ]
               (paramCard4Model AxiGluon) ) ++ "\n\n\n"
paramCardSetup tpath SixFull (SixFullParam m g gd) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("massSix" , (printf "%.4e" m :: String))
               , ("gRSix"   , (printf "%.4e" g :: String))
               , ("gRSixD"  , (printf "%.4e" gd :: String))
               , ("widthSix", (printf "%.4e" (decayWidthExotic g m mtop) :: String)) ]
               (paramCard4Model SixFull) ) ++ "\n\n\n"
 -- Decay width is not right. 
paramCardSetup tpath TripFull (TripFullParam m g gd) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("massTrip" , (printf "%.4e" m :: String))
               , ("gRtrip"   , (printf "%.4e" g :: String))
               , ("gRtripD"  , (printf "%.4e" gd :: String))
               , ("widthTrip", (printf "%.4e" (decayWidthExotic g m mtop) :: String)) ]
               (paramCard4Model TripFull) ) ++ "\n\n\n"
 -- Decay width is not right. 
paramCardSetup tpath WpZpFull (WpZpFullParam mWp' mZp' gWpdt' gWpub' gZpbb'
                                             gZptt' gZpuu' gZpdd' ) = do 
  templates <- directoryGroup tpath
  let (gammaWp,gammaZp) = gammaWpZpFullBelowTopMass mWp' mZp' gWpub' gZpuu' gZpdd' gZpbb'  
  return $ ( renderTemplateGroup
               templates
               [ ("mWp"         , (printf "%.4e" mWp'   :: String))
               , ("mZp"         , (printf "%.4e" mZp'   :: String))
               , ("gWpdtOverSqrt2", (printf "%.4e" (gWpdt' / sqrt 2) :: String))
               , ("gWpubOverSqrt2", (printf "%.4e" (gWpub' / sqrt 2) :: String))
               , ("gZpbb"       , (printf "%.4e" gZpbb' :: String))
               , ("gZptt"       , (printf "%.4e" gZptt' :: String))
               , ("gZpuu"       , (printf "%.4e" gZpuu' :: String))
               , ("gZpdd"       , (printf "%.4e" gZpdd' :: String)) 
               , ("wWp"         , (printf "%.4e" gammaWp :: String)) 
               , ("wZp"         , (printf "%.4e" gammaZp :: String))
               ]
               (paramCard4Model WpZpFull)  ) ++ "\n\n\n"

--               , ("gRWpoverSqrtTwo", (printf "%.4e" (g / (sqrt 2.0)) :: String))
--               , ("gRWpUoverSqrtTwo", (printf "%.4e" (gu / (sqrt 2.0)) :: String))
--               , ("widthWp"      , (printf "%.4e" (gammaWpZp m g) :: String)) ]

 -- Decay width is not right. 
paramCardSetup _ _ _ = error "No matching param card type" 