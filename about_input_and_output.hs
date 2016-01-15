--main = putStrLn "hello, world"


--main = do 
--	putStrLn "Hello, what's your name?"
--	name <- getLine
--	putStrLn ("Hey " ++ name ++ ", you rock!")




--import Data.Char

--main = do
--	putStrLn "What's your first name?"
--	firstName <- getLine
--	putStrLn "What's your last name?"
--	lastName <- getLine
--	let 
--		bigFirstName = map toUpper firstName
--		bigLastName = map toUpper lastName
--		--------------------------------------
--		----indentation is important in Haskell
--	putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"




--main = do 
--	line <- getLine 
--	if null line 
--		then return ()
--		else do
--			putStrLn $ reverseWords line
--			main

--reverseWords :: String -> String
--reverseWords = unwords . map reverse . words 



--main = do
--	a <- return "hell"
--	b <- return "yeah"
--	putStrLn $ a ++ " " ++ b

---------return is the opposite of <-   ----------

--main = do 	
--	putStr "Hey, "
--	putStr "I'm "
--	putStrLn "Andy!"

---------indentation is key important!------------

--main = do
--	print True
--	print 2
--	print "haha"
--	print 3.2
--	print [3,4,4]

-----------print = putStrLn . show -----------------

--main = do     
--    c <- getChar  
--    if c /= ' '  
--        then do  
--            putChar c  
--            main  
--        else return ()  

--import Control.Monad

--main = do
--	c <- getChar
--	when (c /= ' ') $ do
--		putChar c
--		main 

--main = do 
--	rs <- sequence [getLine, getLine, getLine]
--	print $ unwords rs

--import Control.Monad  
--import Data.Char  
  
--main = forever $ do  
--    putStr "Give me some input: "  
--    l <- getLine  
--    putStrLn $ map toUpper l  

--import Control.Monad

--main = do
--	colors <- forM [1,2,3,4] (\a -> do
--		putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
--		color <- getLine
--		return color)
--	putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
--	mapM putStrLn colors
	
----Files and stream

--import Control.Monad
--import Data.Char

--main = forever $ do
--	putStr "Give me some input: "
--	l <- getLine
--	putStrLn $ map toUpper l

--import Data.Char

--main = do
--	contents <- getContents
--	putStr (map toUpper contents)

--main = do
--	contents <- getContents
--	putStr (shortLinesOnly contents)

--shortLinesOnly :: String -> String
--shortLinesOnly input = 
--	let 
--		allLines = lines input
--		shortLines = filter (\line -> length line < 10) allLines
--		result = unlines shortLines
--	in 	result

--main = interact $ unlines . filter ((<10) . length) . lines

--respondPalindromes = unlines . map (\xs -> 
--	if isPalindrome xs then "palindrome" else "not palindrome") . lines
--	where isPalindrome xs = xs == reverse xs

--main = interact respondPalindromes


import System.IO 

main = do 
	handle <- openFile "tmp.txt" ReadMode
	contents <- hGetContents handle
	putStr contents
	hClose handle

------------------------------------------------------