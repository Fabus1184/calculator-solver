module Main where

import Control.Applicative ((<|>))
import Control.Lens (element, (&), (.~))
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Trans.State (State, evalState, get, put, runState)
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (find, intercalate, isInfixOf, isPrefixOf)
import Data.List.Extra ((!?))
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (pack, replace, unpack)
import Debug.Trace (trace, traceId, traceShow, traceShowId)
import GHC.TopHandler (flushStdHandles)
import Text.Read (readEither, readMaybe)

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
    | Replace
        { a :: String
        , b :: String
        }
    | Sum
    | Pow
        { x :: Int
        }
    | RotR
    | RotL
    | Mirror
    | Error
    | MetaPlus
        { x :: Int
        }
    | Store
    | Recall
        { z :: Maybe Int
        }
    | Inv10
    deriving (Eq)

instance Read Button where
    readsPrec _ [] = undefined
    readsPrec n (' ' : xs) = readsPrec n xs
    readsPrec n s
        | "+/-" `isPrefixOf` s = [(Mult (-1), drop 3 s)]
        | isDigit (head s) = do
            let (n, xs') : _ = readParen False reads s
            [(N n, xs')]
        | "Rev" `isPrefixOf` s = [(Rev, drop 3 s)]
        | "<<" `isPrefixOf` s = [(Shl, drop 2 s)]
        | "Sum" `isPrefixOf` s = [(Sum, drop 3 s)]
        | "Store" `isPrefixOf` s = [(Store, drop 5 s)]
        | "[+]" `isPrefixOf` s = do
            let (n, xs') : _ = readParen False reads (drop 3 s)
            [(MetaPlus n, xs')]
        | "Mirror" `isPrefixOf` s = [(Mirror, drop 6 s)]
        | "Inv10" `isPrefixOf` s = [(Inv10, drop 5 s)]
        | head s == '>' = [(RotR, drop 1 s)]
        | head s == '<' = [(RotL, drop 1 s)]
        | otherwise = readsPrec' n s
      where
        readsPrec' _ ('(' : xs) = do
            let ((a, b), xs') : _ =
                    reads ('(' : xs)
                        <|> ((\(((a, b), xs) : _) -> [((show a, show b), xs)]) . (reads :: String -> [((Int, Int), String)]) $ '(' : xs)
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

instance Show Button where
    show (Plus x) = "+ " ++ show x
    show (Minus x) = "- " ++ show x
    show (Mult (-1)) = "+/-"
    show (Mult x) = "x " ++ show x
    show (Div x) = "/ " ++ show x
    show Rev = "Reverse"
    show Shl = "<<"
    show (N x) = show x
    show (Replace a b) = "(" ++ a ++ " => " ++ b ++ ")"
    show Sum = "Sum"
    show (Pow x) = "x^" ++ show x
    show RotL = "Shift <"
    show RotR = "Shift >"
    show Mirror = "Mirror"
    show Error = "ERROR"
    show (MetaPlus x) = "[+] " ++ show x
    show Store = "Store"
    show (Recall (Just x)) = "_" ++ show x ++ "_"
    show (Recall Nothing) = "_Nothing_"
    show Inv10 = "Inv10"

applyMeta :: (Int -> Int) -> Button -> Button
applyMeta f (Plus x) = Plus $ f x
applyMeta f (Minus x) = Minus $ f x
applyMeta f (Mult x) = Mult $ f x
applyMeta f (Div x) = Div $ f x
applyMeta f (N x) = N $ f x
applyMeta f (Replace a b) = Replace (show . f . read $ a) (show . f . read $ b)
applyMeta f (Pow x) = Pow $ f x
applyMeta f (Recall (Just x)) = Recall . Just $ f x
applyMeta _ x = x

cpn :: [a] -> Int -> [[a]]
cpn _ 0 = []
cpn x 1 = map (: []) x
cpn x 2 = [[a, b] | a <- x, b <- x]
cpn x n = [a : b | a <- x, b <- cpn x (n - 1)]

eval :: [Int] -> Maybe (Int, Int) -> State ([Button], Maybe Int) [String]
eval [] _ = pure []
eval (b : bs) ps = do
    (gr, acc) <- get

    -- execute f
    let acc' = acc >>= f (gr !! b)

    let acc'' = acc' >>= portals ps

    put (gr, acc'')

    -- update grid (meta or store buttons)
    f' $ gr !! b

    (gr', _) <- get

    -- append string to output
    eval bs ps <&> (++) [show (gr !! b)]
  where
    portals :: Maybe (Int, Int) -> Int -> Maybe Int
    portals (Just (i, o)) x
        | maybe False isDigit (reverse (show x) !? i) = do
            let di = read [reverse (show x) !! i] :: Int
            let t = read . reverse . (\x -> take i x ++ drop (i + 1) x) $ reverse (show x) & element i .~ '0' :: Int
            let r = t + (di * (10 ^ o))
            if r == x then traceShow r Nothing else portals (Just (i, o)) r
        | otherwise = Just x
    portals Nothing x = Just x

    f' :: Button -> State ([Button], Maybe Int) String
    f' Store = do
        (x, y) <- get
        put (map (store y) x, y)
        pure []
    f' (MetaPlus x) = do
        (gr, acc) <- get
        let gr' = map (applyMeta (+ x)) gr
        put (gr', acc)
        pure []
    f' x = pure []

    store :: Maybe Int -> Button -> Button
    store x (Recall _) = Recall x
    store _ y = y

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

f :: Button -> (Int -> Maybe Int)
f (Plus x) = check . (+) x
f (Minus x) = check . flip (-) x
f (Mult x) = check . (*) x
f (Div x) = flip div' x
f Rev = check . rev
f Shl = check . (<<)
f (N x) = check . __ x
f (Replace a b) = \x -> if a `isInfixOf` show x then check . read . unpack . replace (pack a) (pack b) . pack . show $ x else Nothing
f Sum = \x -> case () of
    _
        | x < 0 -> fmap ((*) (-1) . sum) <$> mapM (\x -> check $ read [x]) . show . (*) (-1) $ x
        | otherwise -> fmap sum <$> mapM (\x -> check $ read [x]) . show $ x
f (Pow x) = check . flip (^) x
f RotR = \x ->
    let i = x * (if x < 0 then (-1) else 1)
     in check . (*) (if x < 0 then (-1) else 1) . read $ last (show i) : init (show i)
f RotL = \x ->
    let i = x * (if x < 0 then (-1) else 1)
     in check . (*) (if x < 0 then (-1) else 1) . read $ tail (show i) ++ [head (show i)]
f Mirror = \x ->
    let i = x * (if x < 0 then (-1) else 1)
     in check . (*) (if x < 0 then (-1) else 1) . read $ show i ++ (reverse . show $ i)
f Error = const Nothing
f (MetaPlus x) = check
f (Recall (Just x)) = \y -> check . (*) (signum x * signum y) . read $ (show (abs y) ++ show (abs x))
f (Recall Nothing) = const Nothing
f Store = check
f Inv10 = \x -> check . (*) (if x < 0 then (-1) else 1) . read . concatMap (\y -> show $ (10 - read [y]) `mod` 10) . show . abs $ x

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
            Just bs <- liftIO getLine <&> readMaybe :: MaybeT IO (Maybe [Button])

            liftIO $ putStr "Portals: "
            liftIO flushStdHandles
            ps <- liftIO getLine <&> readMaybe :: MaybeT IO (Maybe (Int, Int))

            let bs' = if Store `elem` bs then bs ++ [Recall Nothing] else bs

            let y =
                    find (const True)
                        . filter (\(_, (_, y)) -> y == Just g)
                        . map (\y -> runState (eval y ps) (bs', Just s))
                        . cpn [0 .. length bs' - 1]
                        $ n
            --liftIO . mapM_ print . filter (\(_, (_, x)) -> isJust x) . map (\y -> runState (eval y) (bs, Just s)) . cpn [0 .. length bs - 1] $ n
            liftIO $ putStrLn ""
            liftIO . putStrLn $ maybe "No Solution" ((++) "Solution: " . intercalate " | " . fst) y
    main
