module HEP.Automation.MadGraph.Cluster where 

data ClusterRunType = NoParallel 
                    | Parallel Int 
                    | Cluster (WorkSetup a -> String) -- ^ cluster directory naming function
                    deriving Show


