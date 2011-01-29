module Model where

import Text.Printf

import Text.StringTemplate
import Text.StringTemplate.Helpers


data Model = Wp | ZpH | Six | Trip

data ModelVersion = MadGraph4 | MadGraph5



data MassCoupParamSet = M600C3_65 | M600C4_6  | M400C3_6 
                      | M200C1_0  | M300C1_0  | M400C1_0  | M600C1_0
                      | M400C3_45 | M400C3_3  | M400C3_15  
                      | M600C4_4 | M600C4_2 | M600C4_0
                      | M600C3_5  | M600C3_35 | M600C3_2
  
massExotic :: MassCoupParamSet -> Double
--massExotic M600C3_65 = 600.0 
--massExotic M600C4_6  = 600.0
--massExotic M400C3_6  = 400.0 
--massExotic M200C1_0  = 200.0 
--massExotic M300C1_0  = 300.0 
--massExotic M400C1_0  = 400.0 
--massExotic M600C1_0  = 600.0 
massExotic M400C3_45 = 400.0 
massExotic M400C3_3  = 400.0  
massExotic M400C3_15 = 400.0   
massExotic M600C4_4  = 600.0  
massExotic M600C4_2  = 600.0 
massExotic M600C4_0  = 600.0 
massExotic M600C3_5  = 600.0 
massExotic M600C3_35 = 600.0 
massExotic M600C3_2  = 600.0


coupExotic :: MassCoupParamSet -> Double 
--coupExotic M600C3_65 = 3.65 
--coupExotic M600C4_6  = 4.6
--coupExotic M400C3_6  = 3.6 
--coupExotic M200C1_0  = 1.0 
--coupExotic M300C1_0  = 1.0 
--coupExotic M400C1_0  = 1.0 
--coupExotic M600C1_0  = 1.0 
coupExotic M400C3_45 = 3.45  
coupExotic M400C3_3  = 3.3 
coupExotic M400C3_15 = 3.15  
coupExotic M600C4_4  = 4.4  
coupExotic M600C4_2  = 4.2
coupExotic M600C4_0  = 4.0 
coupExotic M600C3_5  = 3.5 
coupExotic M600C3_35 = 3.35
coupExotic M600C3_2  = 3.2


decayWidthExotic :: Param -> Double  
--decayWidthExotic M600C3_65 = 133.32
--decayWidthExotic M600C4_6  = 211.75
--decayWidthExotic M400C3_6  = 67.685
--decayWidthExotic M200C1_0  = 0.23012
--decayWidthExotic M300C1_0  = 2.6191
--decayWidthExotic M400C1_0  = 5.2227
--decayWidthExotic M600C1_0  = 10.007
decayWidthExotic (TripParam M400C3_45) = 62.162
decayWidthExotic (TripParam M400C3_3 ) = 56.875
decayWidthExotic (TripParam M400C3_15) = 51.822
decayWidthExotic (TripParam M600C4_4 ) = 193.73
decayWidthExotic (TripParam M600C4_2 ) = 176.52
decayWidthExotic (TripParam M600C4_0 ) = 160.11
decayWidthExotic (SixParam  M600C3_5 ) = 122.59
decayWidthExotic (SixParam  M600C3_35) = 112.30
decayWidthExotic (SixParam  M600C3_2 ) = 102.47


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
               , ("widthSix"      , (printf "%.4e" (decayWidthExotic (SixParam set)) :: String)) ]
               (paramCard4Model Six) ) ++ "\n\n\n"
paramCardSetup tpath Trip (TripParam set) = do 
  templates <- directoryGroup tpath 
  return $ ( renderTemplateGroup
               templates
               [ ("massTrip"      , (printf "%.4e" (massExotic set) :: String))
               , ("gRtrip"        , (printf "%.4e" (coupExotic set) :: String))
               , ("widthTrip"      , (printf "%.4e" (decayWidthExotic (TripParam set)) :: String)) ]
               (paramCard4Model Trip) ) ++ "\n\n\n"

