module GlobalFunctions where

import Data.Maybe

parseInt :: String -> Maybe Int
parseInt str = case ((reads str)::[(Int,[Char])]) of
	[(num,"")] -> Just num
	_ -> Nothing
	
askRec :: (Maybe a) -> (IO a) -> String ->(IO a)
askRec value handler errorMessage = case value of 
	(Just a) -> (return a)
	Nothing -> putStrLn errorMessage >> handler
	
	
data ChatMessage = USER_MESSAGE | NICK_REQUEST | NICK_ANSWER



addElement e lst= e:lst;
showLst lst = putStrLn lst;
delByPos lst pos = (take (pos-1) lst)++(drop pos lst);
getByPos lst pos = lst !! pos;
setByPos lst pos val = (take (pos-1) lst)++[val]++(drop pos lst)



