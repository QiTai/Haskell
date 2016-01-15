
--data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

--surface :: Shape -> Float
--surface (Circle _ _ r) = pi * r ^ 2
--surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
module Shape
( 	Point(..),
	Shape(..),
	surface,
	nudge
) where

import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float 
surface (Circle _ r) = pi * r * 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

--data Person = Person String String Int Float String String deriving (Show)	--unreadable

--firstName :: Person -> String
--firstName (Person firstname _ _ _ _ _) = firstname

--lastName :: Person -> String
--lastName (Person _ lastName _ _ _ _) = lastName

--age :: Person -> Int
--age (Person _ _ age _ _ _) = age

--height :: Person -> Float
--height (Person _ _ _ height _ _) = height

--phoneNumber :: Person -> String
--phoneNumber (Person _ _ _ _ number _) = number

--flavor :: Person -> String
--flavor (Person _ _ _ _ _ flavor) = flavor 								--cumbersome

--data Person = Person {firstName 	:: String,
--			          lastName  	:: String,
--			          age			:: Int,
--			          height 		:: Float,
--			          phoneNumber	:: String,
--			          flavor 		:: String
--					} deriving (Show) 


--data Maybe a = Nothing | Just a


--data Car = Car {
--				company :: String,
--				model 	:: String,
--				year	:: Int
--			   } deriving (Show)

--change former construtor to this
data Car a b c = Car { company 	:: a
					 , model	:: b
					 , year		:: c	
					 } deriving (Show)

tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  


data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

data Person = Person {
	firstName :: String,
	lastName  :: String,
	age		  :: Int
} deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
		deriving (Eq, Ord, Show, Read, Bounded, Enum)



------------------------------------------------------------
------------------type synonyms-----------------------------
--type PhoneBook = [(String, String)]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssoList k v = [(k, v)]

--type IntMap v = Map Int v

type IntMap = Map.Map Int
---------------------------------------------------------------

--data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
	case Map.lookup lockerNumber map of 
		Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
		Just (state, code) -> if state /= Taken
								then Right code 
								else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"


lockers :: LockerMap
lockers = Map.fromList [(100, (Taken, "222")),
						(101, (Free, "23"))
					   ]


---------------------------------------------------------------------
-------------------------recursive data structures-------------------

--data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

--data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)


infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x < a = Node a (treeInsert x left) right
	| x > a = Node a left (treeInsert x right)

-------------------------------------------------------------
--------------------------class-----------------------------
--class Eq' a where
--	(==) :: a -> a -> Bool
--	(/=) :: a -> a -> Bool
--	x == y = not (x /= y)
--	x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
	Red == Red = True
	Green == Green = True
	Yellow == Yellow = True
	_ == _ = False

--instance (Eq m) => Eq (Maybe m) where
--	Just x == Just y = x == y
--	Nothing == Nothing = True
--	_ == _ = False

instance Show TrafficLight where
	show Red = "Red Light"
	show Yellow = "Yellow Light"
	show Green = "Green Light"


class YesNo a where
	yesno :: a -> Bool

instance YesNo Int where
	yesno 0 = False
	yesno _ = True

instance YesNo [a] where
	yesno [] = False
	yesno _ = True

instance YesNo Bool where
	yesno = id

instance YesNo (Maybe a) where
	yesno (Just _) = True
	yesno Nothing = False

instance YesNo (Tree a) where
	yesno EmptyTree = False
	yesno _ = True

instance YesNo TrafficLight where
	yesno Red = False
	yesno _ = True


yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

--------------------------------------------------------
---------------------Functor typeclasses---------------

--class Functor f where
--	fmap :: (a -> b) -> f a -> f b


--instance Functor [] where
--	fmap = map

--instance Functor Maybe where
--	fmap f (Just x) = Just (f x)
--	fmap f Nothing = Nothing

instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)


instance Functor (Either a) where
	fmap f (Right x) = Right (f x)
	fmap f (Left x) = Left x
	
























