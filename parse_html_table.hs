import Data.List
import Text.HTML.TagSoup
import System.Environment 
import Control.Monad.State
import Text.Printf


isTagOpenTr (TagOpen "tr" _) = True
isTagOpenTr _ = False


isTagCloseTr (TagClose "tr") = True
isTagCloseTr _ = False


firstChunk :: State [Tag String] [Tag String]
firstChunk = do currlst <- get
                let (_,lst1) = break isTagOpenTr currlst
                 in  if null lst1
                       then do put []
                               return []
                       else let (lst2,lst3) = break isTagCloseTr $ tail lst1
                            in  if null lst3
                                then do put []
                                        return lst2
                                else do put $ tail lst3
                                        return lst2
                     

matchpattern [TagText _, TagOpen _ _, TagOpen _ _, TagText _, TagClose _, TagText _, TagOpen _ _, TagText _, TagClose _, TagText _, TagOpen _ _, TagOpen _ _, TagText _, TagClose _, TagOpen _ _, TagText _, TagClose _, TagClose _] = True 
matchpattern _ = False 

extract_name_email [TagText _, TagOpen _ _, TagOpen _ x, TagText y, TagClose _, TagText _, TagOpen _ _, TagText _, TagClose _, TagText _, TagOpen _ _, TagOpen _ _, TagText _, TagClose _, TagOpen _ _, TagText _, TagClose _, TagClose _] 
  = let [(_,_),(_,_),(_,email)] = x 
        name = polish y
    in  (name,email)
extract_name_email _ = error "strange pattern"


polish name = (tail.tail) name
  
format_name_email (name,email) = printf "%-40s :   %-25s" name email

main = do 
  args <- getArgs
  let filename = args !! 0
  src <- readFile filename
  let listparsed =  parseTags src
      chunkparsed = evalState (sequence (repeat firstChunk)) listparsed
      chunkparsed_finite = takeWhile (not.null) chunkparsed
      matched = filter matchpattern  chunkparsed_finite 
      name_email_lst = map extract_name_email matched
  putStrLn $ intercalate "\n" $ map format_name_email name_email_lst 
  