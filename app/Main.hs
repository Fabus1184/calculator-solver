module Main where

import Control.Monad (guard, liftM, unless, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Trans.State (State, evalState, get, put, runState)
import Data.Char (isDigit)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.List (find, intercalate, isPrefixOf)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (pack, replace, unpack)
import Debug.Trace (trace, traceId, traceShow, traceShowId)
import GHC.TopHandler (flushStdHandles)
import Text.Read (readEither, readMaybe)
import Unsafe.Coerce (unsafeCoerce)

type ButtonI = Button Int

data Button a
    = Plus
        { x :: a
        }
    | Minus
        { x :: a
        }
    | Mult
        { x :: a
        }
    | Div
        { x :: a
        }
    | Rev
    | Shl
    | N
        { x :: a
        }
    | Replace
        { x :: a
        , y :: a
        }
    | Sum
    | Pow
        { x :: a
        }
    | RotR
    | RotL
    | Mirror
    | Error
    | MetaPlus
        { x :: a
        }
    deriving (Eq)

instance (Read a, Num a) => Read (Button a) where
    readsPrec _ [] = undefined
    readsPrec n (' ' : xs) = readsPrec n xs
    readsPrec n s
        | "+/-" `isPrefixOf` s = [(Mult (-1), drop 3 s)]
        | isDigit (head s) = do
            let (n, xs') : _ = readParen False reads s
            [(N n, xs')]
        | "rev" `isPrefixOf` s = [(Rev, drop 3 s)]
        | "<<" `isPrefixOf` s = [(Shl, drop 2 s)]
        | "sum" `isPrefixOf` s = [(Sum, drop 3 s)]
        | "[+]" `isPrefixOf` s = do
            let (n, xs') : _ = readParen False reads (drop 3 s)
            [(MetaPlus n, xs')]
        | head s == '>' = [(RotR, drop 1 s)]
        | head s == '<' = [(RotL, drop 1 s)]
        | head s == 'M' = [(Mirror, drop 1 s)]
        | otherwise = readsPrec' n s
      where
        readsPrec' _ ('(' : xs) = do
            let ((a, b), xs') : _ = reads $ '(' : xs
            [(Replace a b, xs')]
        readsPrec' _ ('+' : xs) = do
            let (n, xs') : _ = readParen False reads xs
            [(Plus n, xs')]
        readsPrec' _ ('-' : xs) = do
            let (n, xs') : _ = readParen False reads xs
            [(Minus n, xs')]
        readsPrec' _ ('x' : xs) = do
            let (n, xs') : _ = readParen False reads xs
            [(Mult n, xs')]
        readsPrec' _ ('/' : xs) = do
            let (n, xs') : _ = readParen False reads xs
            [(Div n, xs')]
        readsPrec' _ ('^' : xs) = do
            let (n, xs') : _ = readParen False reads xs
            [(Pow n, xs')]
        readsPrec' _ s = [(Error, s)]

instance (Num a, Eq a, Show a) => Show (Button a) where
    show (Plus x) = "+" ++ show x
    show (Minus x) = "-" ++ show x
    show (Mult (-1)) = "+/-"
    show (Mult x) = "x" ++ show x
    show (Div x) = "/" ++ show x
    show Rev = "rev"
    show Shl = "<<"
    show (N x) = show x
    show (Replace a b) = "(" ++ show a ++ " => " ++ show b ++ ")"
    show Sum = "sum"
    show (Pow x) = "x^" ++ show x
    show RotL = "Shift <"
    show RotR = "Shift >"
    show Mirror = "Mirror"
    show Error = "ERROR"
    show (MetaPlus x) = "[+]" ++ show x

-- use of "unsafeCoerce", very bad!
instance Functor Button where
    fmap f (Plus x) = Plus $ f x
    fmap f (Minus x) = Minus $ f x
    fmap f (Mult x) = Mult $ f x
    fmap f (Div x) = Div $ f x
    fmap f Rev = Rev
    fmap f Shl = Shl
    fmap f (N x) = N $ f x
    fmap f (Replace a b) = Replace (f a) (f b)
    fmap f Sum = Sum
    fmap f (Pow x) = Pow $ f x
    fmap f RotL = RotL
    fmap f RotR = RotR
    fmap f Mirror = Mirror
    fmap f Error = Error
    fmap f (MetaPlus x) = MetaPlus $ unsafeCoerce x

cpn :: [a] -> Int -> [[a]]
cpn _ 0 = []
cpn x 1 = map (: []) x
cpn x 2 = [[a, b] | a <- x, b <- x]
cpn x n = [a : b | a <- x, b <- cpn x (n - 1)]

eval :: [Int] -> State ([ButtonI], Maybe Int) [String]
eval [] = pure []
eval (b : bs) = do
    (gr, _) <- get
    -- update grid if meta button pressed
    f' $ gr !! b

    (gr, acc) <- get

    -- execute f
    let acc' = acc >>= f (gr !! b)
    put (gr, acc')

    -- append string to output
    eval bs <&> (++) [show (gr !! b)]
  where
    f' :: ButtonI -> State ([ButtonI], Maybe Int) String
    f' (MetaPlus x) = do
        (gr, acc) <- get
        let gr' = map (fmap (+ x)) gr
        put (gr', acc)
        pure ""
    f' x = pure ""

div' :: Int -> Int -> Maybe Int
div' x y
    | x `mod` y == 0 = check $ x `div` y
    | otherwise = Nothing

(+/-) :: Int -> Int
(+/-) x = x * (-1)

(<<) :: Int -> Int
(<<) x = x `div` 10

rev :: Int -> Int
rev x
    | x >= 0 = read . reverse . show $ x
    | otherwise = (*) (-1) . read . reverse . show . (*) (-1) $ x

__ :: Int -> Int -> Int
__ n x = ((10 ^ length (show n)) * x) + n

check :: Int -> Maybe Int
check x
    | length (show x) > 6 = Nothing
    | otherwise = Just x

f :: ButtonI -> (Int -> Maybe Int)
f (Plus x) = check . (+) x
f (Minus x) = check . flip (-) x
f (Mult x) = check . (*) x
f (Div x) = flip div' x
f Rev = check . rev
f Shl = check . (<<)
f (N x) = check . __ x
f (Replace a b) = check . read . unpack . replace (pack . show $ a) (pack . show $ b) . pack . show
f Sum = \x -> case () of
    _
        | x < 0 -> fmap ((*) (-1) . sum) <$> mapM (\x -> check $ read [x]) . show . (*) (-1) $ x
        | otherwise -> fmap sum <$> mapM (\x -> check $ read [x]) . show $ x
f (Pow x) = check . flip (^) x
f RotR = \x -> let i = x * (if x < 0 then (-1) else 1) in check . (*) (if x < 0 then (-1) else 1) . read $ last (show i) : init (show i)
f RotL = \x -> let i = x * (if x < 0 then (-1) else 1) in check . (*) (if x < 0 then (-1) else 1) . read $ tail (show i) ++ [head (show i)]
f Mirror = \x -> let i = x * (if x < 0 then (-1) else 1) in check . (*) (if x < 0 then (-1) else 1) . read $ show i ++ (reverse . show $ i)
f Error = undefined
f (MetaPlus x) = check

main :: IO ()
main = do
    fmap (fromMaybe ()) $
        runMaybeT $ do
            liftIO $ putStr "Start: "
            liftIO flushStdHandles
            Just s <- liftIO getLine <&> readMaybe :: MaybeT IO (Maybe Int)

            liftIO $ putStr "Goal: "
            liftIO flushStdHandles
            Just g <- liftIO getLine <&> readMaybe :: MaybeT IO (Maybe Int)

            liftIO $ putStr "Moves: "
            liftIO flushStdHandles
            Just n <- liftIO getLine <&> readMaybe :: MaybeT IO (Maybe Int)

            liftIO $ putStr "Buttons: "
            liftIO flushStdHandles
            Just bs <- liftIO getLine <&> readMaybe :: MaybeT IO (Maybe [ButtonI])

            let x = map (\y -> runState (eval y) (bs, Just s)) (cpn [0 .. length bs - 1] n)
            Just (y, _) <- pure $ find (const True) . filter (\y -> (snd . snd) y == Just g) $ x
            liftIO $ putStrLn $ intercalate " | " y
    main
