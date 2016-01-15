removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z']]

--Pattern Matching
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY"
lucky x = "Sorry"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Sorry, should not use empty list"
head' (a: _) = a

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++  all ++ " is " ++ [x]


max' :: (Ord a) => a -> a -> a
max' a b
	| a > b 	= a
	| otherwise = b

--max' :: (Ord a) => a -> a -> a
--max' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b | a > b = GT | a == b = EQ | otherwise = LT 


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| bmi <= skinny = "You're underweight, you emo, you!"
	| bmi <= normal = "You're supposedly normal."
	| bmi <= fat = "You're fat! Lose some weight"
	| otherwise = "You're a whale"
	where 
		bmi = weight / height ^ 2  
		(skinny, normal, fat) = (18.5, 25.0, 30.0)  



initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where 
		(f:_) = firstname
		(l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
	let 
		sideArea = 2 * pi * r * h
		topArea  = pi * r ^2
	in 	sideArea + 2 * topArea


describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of 
												[] -> "empty."
											   	[x] -> "a singleton list."
											   	xs -> "a longer list."


describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
	where 
		what [] = "empty." 
		what [x] = "a singleton list."
		what xs = "a longer list." 

















