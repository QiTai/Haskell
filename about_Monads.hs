import Data.Monoid
import Data.List
import Control.Monad.Writer 
import Control.Monad.Error
import Control.Monad.State
import Data.Ratio
-------------------------------------------
--class Monad m where
--	return :: a -> m a

--	(>>=) :: m a -> (a -> m b) -> m b

--	(>>) :: m a -> m b -> m b
--	x >> y = x >> \_ -> y

--	fail :: String -> m a 
--	fail msg = error msg


--------------------------------------------
--instance Monad Maybe where
--	return x = Just x
--	Nothing >>= f = Nothing
--	Just x >>= f = f x
--	fail _ = Nothing
	

--------------------------------------------

type Birds = Int 
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
	| abs ((left + n) - right) < 4 = Just (left + n, right)
	| otherwise 				   = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
	| abs (left - (right + n)) < 4 = Just (left, right + n)
	| otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

--can replace banana with >> Nothing

----------------------------------------------

--instance Monad [] where
--	return x = [x]
--	xs >>= f = concat (map f xs)
--	fail _ = [] 


----------------------------------------------

--class Monad m => MonadPlus m where
--	mzero :: m a
--	mplus :: m a -> m a -> m a 

--instance MonadPlus [] where
--	mzero = []
--	mplus = (++)

--guard :: (MonadPlus m) => Bool -> m ()
--guard True = return ()
--guard False = mzero

-----------------------------------------------
--type KnightPos = (Int, Int)

--moveKnight :: KnightPos -> [KnightPos]
--moveKnight (c,r) = do
--	(c',r') <- [(c+2, r+1), (c+2, r-1), (c-2, r+1), (c-2, r-1),
--				(c+1, r+2), (c+1, r-2), (c-1, r+2), (c-1, r-2)]
--	guard (c' `elem` [1..8] && r' `elem` [1..8])
--	return (c',r')

--in3 :: KnightPos -> [KnightPos]
--in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

--canReachIn3 :: KnightPos -> KnightPos -> Bool
--canReachIn3 end start = end `elem` in3 start
-------------------------------------------------

--(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
--f <=< g = (\x -> g x >>= f)

-------------------------------------------------
---------------For a Few Monads More-------------
-------------------------------------------------
--applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
--applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
--applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, newLog `mappend` log)


type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ 		 = ("beer", Sum 30)

-----in Control.Monad.Writer
--newtype Writer w a = Writer { runWriter :: (a, w) }

--instance (Monoid w) => Monad (Writer w) where
--	return x = Writer (x, mempty)
--	(Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
	
-----------------------------------------

logNum :: Int -> Writer [String] Int
logNum x = writer (x, ["Got number: " ++ show x])

multwithLog :: Writer [String] Int
multwithLog = do
	x <- logNum 3
	y <- logNum 5
	return (x*y)

-----------------------------------------
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }


toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
	mempty = DiffList (\x -> [] ++ x)
	(DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

------------------------------------------
------------state monad-------------------

--newtype State s a = State { runState :: s -> (a,s)}


--instance Monad (State s) where
--	return x = State $ \s -> (x,s)
--	(State h) >>= f = State $ \s -> let 
--										(a, newState) = h s
--										(State g) = f a 
--									in g newState

type Stack = [Int]

--pop :: State Stack Int
--pop = State $ \(x:xs) -> (x,xs)

--push :: Int -> State Stack ()
--push a= State $ \xs -> ((), (a:xs))



---------------------------------------------
--instance (Error e) => Monad (Either e) where
--	return x = Right x
--	Right  x >>= f = f x 
--	Left err >>= f = Left err
--	fail msg = Left msg

--------------------------------------------------------------------------------------
--- use Monad's own tool to realize --------- 
---	Functor and Applicative Functor -----------

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))


ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do 
	f <- mf
	x <- m
	return (f x)

join :: (Monad m) => m (m a) -> m a
join mm = do 
	m <- mm
	m



-------------------m >>= f = join (fmap f m)------------

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

binSmall :: Int -> Int -> Maybe Int
binSmall acc x 
	| x > 9 	= Nothing
	| otherwise = Just (acc + x)

---------------RPN caculator-------------


---------------------------------------------


newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show 

instance Functor Prob where
	fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
	where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

instance Monad Prob where
	return x = Prob [(x, 1%1)]
	m >>= f  = flatten (fmap f m)
	fail _   = Prob [] 

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]








