import qualified Data.Map as Map  
import Data.Char
import Data.List 
import qualified Data.Set as Set
--import sentence must lie before any function

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
	let nlen = length needle
	in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
	

encode :: Int -> String -> String  
encode shift msg = 
    let 
    	ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted  

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs


phoneBook =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"  





































