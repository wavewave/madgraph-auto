module Model where

import Text.Printf

import Text.StringTemplate
import Text.StringTemplate.Helpers


data Model = Wp | ZpH | Six | Trip

data ModelVersion = MadGraph4 | MadGraph5



data MassCoupParamSet = M600C3_65 | M600C4_6  | M400C3_6 
                      | M200C1_0  | M300C1_0  | M400C1_0  | M600C1_0
  
massExotic :: MassCoupParamSet -> Double
massExotic M600C3_65 = 600.0 
massExotic M600C4_6  = 600.0
massExotic M400C3_6  = 400.0 
massExotic M200C1_0  = 200.0 
massExotic M300C1_0  = 300.0 
massExotic M400C1_0  = 400.0 
massExotic M600C1_0  = 600.0 

coupExotic :: MassCoupParamSet -> Double 
coupExotic M600C3_65 = 3.65 
coupExotic M600C4_6  = 4.6
coupExotic M400C3_6  = 3.6 
coupExotic M200C1_0  = 1.0 
coupExotic M300C1_0  = 1.0 
coupExotic M400C1_0  = 1.0 
coupExotic M600C1_0  = 1.0 



decayWidthExotic :: MassCoupParamSet -> Double  
decayWidthExotic M600C3_65 = 133.32
decayWidthExotic M600C4_6  = 211.75
decayWidthExotic M400C3_6  = 67.685
decayWidthExotic M200C1_0  = 0.23012
decayWidthExotic M300C1_0  = 2.6191
decayWidthExotic M400C1_0  = 5.2227
decayWidthExotic M600C1_0  = 10.007


data Param = WpParam { massWp :: Double, gRWp :: Double } 
           | ZpHParam { massZp :: Double, gRZp :: Double }
           | SixParam { masscoupSix :: MassCoupParamSet } 
           | TripParam { masscoupTrip :: MassCoupParamSet } 


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

mtop = 174.3           

gamma :: Double -> Double -> Double            
gamma mass coup = 
  let r = mtop^2 / mass^2 
  in  coup^2 / (16.0 * pi) * mass * ( 1.0 - 1.5 * r + 0.5 * r^3 ) 




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
paramCardSetup tpath Six (SixParam set) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("massSix"      , (printf "%.4e" (massExotic set) :: String))
               , ("gRSix"        , (printf "%.4e" (coupExotic set) :: String))
               , ("widthzp"      , (printf "%.4e" (decayWidthExotic set) :: String)) ]
               (paramCard4Model Six) ) ++ "\n\n\n"
paramCardSetup tpath Trip (TripParam set) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("massTrip"      , (printf "%.4e" (massExotic set) :: String))
               , ("gRtrip"        , (printf "%.4e" (coupExotic set) :: String))
               , ("widthTrip"      , (printf "%.4e" (decayWidthExotic set) :: String)) ]
               (paramCard4Model Six) ) ++ "\n\n\n"

