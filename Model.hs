module Model where

import Text.Printf

import Text.StringTemplate
import Text.StringTemplate.Helpers


data Model = Wp | ZpH | Six | Trip

data ModelVersion = MadGraph4 | MadGraph5

mtop :: Double
mtop = 174.3           



decayWidthExotic :: Double -> Double -> Double -> Double
decayWidthExotic  y mphi mt = y^(2 :: Int) / (16.0 * pi )
                              * (mphi^(2 :: Int) - mt^(2 :: Int))^(2 :: Int) 
                              / (mphi^(3 :: Int))


data Param = WpParam { massWp :: Double, gRWp :: Double } 
           | ZpHParam { massZp :: Double, gRZp :: Double }
           | SixParam { massSix :: Double, gRSix :: Double } 
           | TripParam { massTrip :: Double, gRTrip :: Double  } 


modelName :: Model -> String
modelName Wp = "fvwp200_MG"
modelName ZpH = "zHorizontal_MG"
modelName Six = "sextets_fv"
modelName Trip = "triplets_fv"

makeProcessFile :: Model -> ModelVersion -> String -> String -> String
makeProcessFile model modelversion process dirname = 
  let importline = case modelversion of
        MadGraph4 -> "import model_v4 " ++ modelName model
        MadGraph5 -> "import model " ++ modelName model
  in importline ++ "\n" ++ process ++ "\n" ++ "output " ++ dirname ++ "\n\n" 


paramCard4Model :: Model -> String
paramCard4Model Wp   = "param_card_wP.dat"
paramCard4Model ZpH  = "param_card_zHorizontal.dat" 
paramCard4Model Six  = "param_card_six.dat"
paramCard4Model Trip = "param_card_trip.dat" 


gamma :: Double -> Double -> Double            
gamma mass coup = 
  let r = mtop^(2 :: Int)/ mass^(2 :: Int)  
  in  coup^(2 :: Int) / (16.0 * pi) *mass*( 1.0 - 1.5 * r + 0.5 * r^(3 :: Int))




paramCardSetup :: FilePath -> Model -> Param -> IO String
paramCardSetup tpath Wp (WpParam m g) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("massWp"       , (printf "%.4e" m :: String))
               , ("gRWpoverSqrtTwo", (printf "%.4e" (g / (sqrt 2.0)) :: String))
               , ("widthWp"      , (printf "%.4e" (gamma m g) :: String)) ]
               (paramCard4Model Wp)  ) ++ "\n\n\n"
paramCardSetup tpath ZpH (ZpHParam m g) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("masszp"       , (printf "%.4e" m :: String))
               , ("gRoverSqrtTwo"  , (printf "%.4e" (g / (sqrt 2.0)) :: String))
               , ("widthzp"      , (printf "%.4e" (gamma m g) :: String)) ]
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

paramCardSetup _ _ _ = error "No matching param card type" 