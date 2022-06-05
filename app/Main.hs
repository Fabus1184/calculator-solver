module Main where

import Control.Monad ((<=<))
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (intercalate, isPrefixOf)
import Debug.Trace (trace)
import GHC.TopHandler (flushStdHandles)

cpn :: [a] -> Int -> [[a]]
cpn _ 0 = []
cpn x 1 = map (: []) x
cpn x 2 = [[a, b] | a <- x, b <- x]
cpn x n = [a : b | a <- x, b <- cpn x (n - 1)]

eval :: [Int -> Maybe Int] -> Int -> Maybe Int
eval = foldl (<=<) pure

div' :: Int -> Int -> Maybe Int
div' x y
    | x `mod` y == 0 = Just $ x `div` y
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

data Button
    = Plus
        { x :: Int
        }
    | Minus
        { x :: Int
        }
    | Mult
        { x :: Int
        }
    | Div
        { x :: Int
        }
    | Rev
    | Shl
    | N
        { x :: Int
        }
    deriving (Eq)

instance Read Button where
    readsPrec n (' ' : xs) = readsPrec n xs
    readsPrec _ ('+' : xs) = do
        let k = takeWhile isDigit xs
        [(Plus (read k), drop (length k) xs)]
    readsPrec _ ('-' : xs) = do
        let k = takeWhile isDigit xs
        [(Minus (read k), drop (length k) xs)]
    readsPrec _ ('x' : xs) = do
        let k = takeWhile isDigit xs
        [(Mult (read k), drop (length k) xs)]
    readsPrec _ ('/' : xs) = do
        let k = takeWhile isDigit xs
        [(Div (read k), drop (length k) xs)]
    readsPrec _ s
        | isDigit (head s) = do
            let k = takeWhile isDigit s
            [(N (read k), drop (length k) s)]
        | "rev" `isPrefixOf` s = [(Rev, drop 3 s)]
        | "<<" `isPrefixOf` s = [(Shl, drop 2 s)]
    readsPrec _ s = trace s undefined

instance Show Button where
    show (Plus x) = "+" ++ show x
    show (Minus x) = "-" ++ show x
    show (Mult x) = "x" ++ show x
    show (Div x) = "/" ++ show x
    show Rev = "rev"
    show Shl = "<<"
    show (N x) = show x

f :: Button -> (Int -> Maybe Int)
f (Plus x) = Just . (+) x
f (Minus x) = Just . flip (-) x
f (Mult x) = Just . (*) x
f (Div x) = flip div' x
f Rev = Just . rev
f Shl = Just . (<<)
f (N x) = Just . __ x

main :: IO ()
main =
    sequence_ $
        iterate
            ( const $ do
                putStr "Start: "
                flushStdHandles
                s <- getLine <&> read

                putStr "Goal: "
                flushStdHandles
                g <- getLine <&> read

                putStr "Moves: "
                flushStdHandles
                n <- getLine <&> read

                putStr "Buttons: "
                flushStdHandles
                bs <- getLine <&> read

                let k = cpn bs n :: [[Button]]
                putStrLn . intercalate " --> " . reverse . map show . head . filter (\y -> eval (map f y) s == Just g) $ k
            )
            (pure ())
