import Control.Applicative
import Data.Monoid
--import qualified Foldable as F
--class Functor f where
--	fmap :: (a -> b) -> f a -> f b


--instance Functor ((->) r) where 
--	fmap f g = (\x -> f (g x))


--instance Functor (r ->) where
--	fmap f g = (\x -> f (g x))

--instance Functor ((->) r) where 
--	fmap = (.) 


------Using Applicative Functors

--class (Functor f) => Applicative f where
--	pure :: a -> f a
--	(<*>):: f (a -> b) -> f a -> f b

--instance Applicative Maybe where
--	pure = Just
--	Nothing <*> _ = Nothing
--	(Just f) <*> something = fmap f something


--instance Applicative [] where
--	pure x = [x]
--	fs <*> xs = [f x | f <- fs, x <- xs]




--instance Applicative IO where
--	pure = return
--	a <*> b = do
--		f <- a
--		x <- b
--		return (f x)

--myAction :: IO String
--myAction = (++) <$> getLine <*> getLine


--instance Applicative ((->) r) where
--	pure x = (\_ -> x)
--	f <*> g = \x -> f x (g x)


--instance Applicative ZipList where
--	pure x = ZipList (repeat x)
--	ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)


-----------------------------------------
newtype ZipList a = ZipList { getZipList :: [a] }

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
	fmap f (Pair (x,y)) = Pair (f x, y)


-------------------------------------------

--class Monoid m where 
--	mempty :: m
--	mappend :: m -> m -> m
--	mconcat :: [m] -> m
--	mconcat = foldr mappend mempty


--list are monoid
--instance Monoid [a] where
--	mempty = []
--	mappend = (++)


---------------------------------------------
--newtype Product a = Product { getProduct :: a }
--	deriving (Eq, Ord, Read, Show, Bounded)

--instance Num a => Monoid (Product a) where
--	mempty = Product 1
--	Product x `mappend` Product y = Product (x * y)

----------------------------------------------
--newtype Any a = Any { getAny :: a }
--	deriving (Eq, Ord, Read, Show, Bounded)

--instance Bool a => Monoid (Any a) where
--	mempty = Any False
--	Any x `mappend` Any y = Any (x || y)
------------------------------------------------
--instance Monoid Ordering where
--	mempty = EQ 
--	LT `mappend` _ = LT
--	GT `mappend` _ = GT
--	EQ `mappend` y = y

--used to compare two strings, first length, then alphabetically

----------------------------------------------
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

----------------------------------------------
instance Monoid Maybe a where
	mempty = Nothing
	Nothing `mappend` x = x
	x `mappend` Nothing = x
	Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)


---------------------------------------------
newtype First a = First { getFirst :: a }
	deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
	mempty = First Nothing
	First (Just x) `mappend` _ = First (Just x)
	First Nothing `mappend` x = x


---------------------------------------------
--data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

--foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

--instance F.Foldable Tree where
--	foldMap f Empty = mempty
--	foldMap f (Node x l r) = F.foldMap f l `mappend` 
--							 f x 		   `mappend`
--							 F.foldMap f r
							 
























