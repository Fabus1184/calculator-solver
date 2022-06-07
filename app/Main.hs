module Main (main) where

import Control.Applicative ((<|>))
import Control.Lens (element, (&), (.~))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.State (State, evalState, get, put, runState)
import Data.Bool (bool)
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (find, intercalate, isInfixOf, isPrefixOf)
import Data.List.Extra ((!?))
import Data.Text (pack, replace, unpack)
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import System.Console.Pretty (Color (Blue, Cyan, Green, Magenta, Red, White, Yellow), Style (Bold), color, style)
import System.IO.Extra (stdout)
import Text.Read (readMaybe)

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
    | -- only integer division allowed
      Div
        { x :: Int
        }
    | Rev
    | -- shift left
      Shl
    | -- append n to the back
      N
        { x :: Int
        }
    | Replace
        { a :: String
        , b :: String
        }
    | -- digital sum
      Sum
    | Pow
        { x :: Int
        }
    | -- rotate right
      RotR
    | -- rotate left
      RotL
    | Mirror
    | -- this should never occur
      Error
    | -- add to every other buttons value
      MetaPlus
        { x :: Int
        }
    | -- store functionality is split into two buttons, recall will be automatically added in main
      Store
    | Recall
        { z :: Maybe Int
        }
    | -- digital inverse with 10
      Inv10
    deriving (Eq)

instance Read Button where
    -- error with no parse on empty input
    readsPrec _ [] = []
    -- skip spaces between symbols
    readsPrec n (' ' : xs) = readsPrec n xs
    readsPrec n s
        | "+/-" `isPrefixOf` s = [(Mult (-1), drop 3 s)]
        | isDigit (head s) = let (n, xs') : _ = readParen False reads s in [(N n, xs')]
        | "Rev" `isPrefixOf` s = [(Rev, drop 3 s)]
        | "<<" `isPrefixOf` s = [(Shl, drop 2 s)]
        | "Sum" `isPrefixOf` s = [(Sum, drop 3 s)]
        | "Store" `isPrefixOf` s = [(Store, drop 5 s)]
        | "[+]" `isPrefixOf` s = let (n, xs') : _ = readParen False reads (drop 3 s) in [(MetaPlus n, xs')]
        | "Mirror" `isPrefixOf` s = [(Mirror, drop 6 s)]
        | "Inv10" `isPrefixOf` s = [(Inv10, drop 5 s)]
        | head s == '>' = [(RotR, drop 1 s)]
        | head s == '<' = [(RotL, drop 1 s)]
        | otherwise = readsPrec' n s
      where
        readsPrec' _ ('(' : xs) =
            -- parse either a tuple of integers or a tuple of strings
            let ((a, b), xs') : _ =
                    reads
                        ('(' : xs)
                        <|> ( (\(((a, b), xs) : _) -> [((show a, show b), xs)])
                                . (reads :: String -> [((Int, Int), String)])
                                $ '(' : xs
                            )
             in [(Replace a b, xs')]
        readsPrec' _ ('+' : xs) = let (n, xs') : _ = readParen False reads xs in [(Plus n, xs')]
        readsPrec' _ ('-' : xs) = let (n, xs') : _ = readParen False reads xs in [(Minus n, xs')]
        readsPrec' _ ('x' : xs) = let (n, xs') : _ = readParen False reads xs in [(Mult n, xs')]
        readsPrec' _ ('/' : xs) = let (n, xs') : _ = readParen False reads xs in [(Div n, xs')]
        readsPrec' _ ('^' : xs) = let (n, xs') : _ = readParen False reads xs in [(Pow n, xs')]
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

-- when meta button pressed apply function to each button, essentially fmap but without arbitrary types
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

-- all possible arrangements of elements x with length n
cpn :: [a] -> Int -> [[a]]
cpn _ 0 = []
cpn x 1 = map (: []) x
cpn x 2 = [[a, b] | a <- x, b <- x]
cpn x n = [a : b | a <- x, b <- cpn x (n - 1)]

-- evaluate series of buttons
eval :: [Int] -> Maybe (Int, Int) -> State ([Button], Maybe Int) [Button]
eval [] _ = pure []
eval (b : bs) ps = do
    -- get state
    (gr, acc) <- get

    -- mutate state
    let acc' = acc >>= press (gr !! b) >>= portals ps
    put (gr, acc')

    -- update grid (meta or store buttons)
    updateGrid $ gr !! b

    -- append button to output
    eval bs ps
  where
    -- recursively apply portals
    portals (Just (i, o)) x
        | maybe False isDigit (reverse (show x) !? i) = do
            -- digit in portal
            let di = read [reverse (show x) !! i] :: Int
            -- rest of number with ported digit replaced by '0'
            let t = read . reverse . (\x -> take i x ++ drop (i + 1) x) $ reverse (show x) & element i .~ '0' :: Int
            -- new number
            let r = t + (di * (10 ^ o))
            -- detect loops and fail
            if r == x then Nothing else portals (Just (i, o)) r
        | otherwise = Just x
    portals Nothing x = Just x

    -- apply store to each button
    updateGrid Store = do
        (x, y) <- get
        put (map (store y) x, y)
        pure []
    -- apply meta function to each button
    updateGrid (MetaPlus x) = do
        (gr, acc) <- get
        let gr' = map (applyMeta (+ x)) gr
        put (gr', acc)
        pure []
    updateGrid x = pure []

    -- if store is pressed update recall button to new value
    store x (Recall _) = Recall x
    store _ y = y

-- check length
check :: Int -> Maybe Int
check x
    | length (show x) > 6 = Nothing
    | otherwise = Just x

-- function represents change of input when pressing a button, this may fail
press :: Button -> (Int -> Maybe Int)
press (Plus x) = check . (+) x
press (Minus x) = check . flip (-) x
press (Mult x) = check . (*) x
press (Div x) = flip div' x
  where
    -- safe div, fails on non-integer result
    div' x y
        | x `mod` y == 0 = check $ x `div` y
        | otherwise = Nothing
press Rev = check . rev
  where
    rev x
        | x >= 0 = read . reverse . show $ x
        | otherwise = (*) (-1) . read . reverse . show . (*) (-1) $ x
press Shl = check . (`div` 10)
press (N x) = check . __ x
  where
    -- "appends" n to the back
    __ n x = read $ show x ++ show n
press (Replace a b) = \x -> if a `isInfixOf` show x then check . read . unpack . replace (pack a) (pack b) . pack . show $ x else Nothing
press Sum = \x -> fmap ((*) (signum x) . sum) <$> mapM (\x -> check $ read [x]) . show . abs $ x
press (Pow x) = check . flip (^) x
press RotR = \x -> check . (*) (signum x) . read $ last (show (abs x)) : init (show (abs x))
press RotL = \x -> check . (*) (signum x) . read $ tail (show (abs x)) ++ [head (show (abs x))]
press Mirror = \x -> check . (*) (signum x) . read $ show (abs x) ++ (reverse . show $ abs x)
press Error = const Nothing
press (MetaPlus x) = check
press (Recall (Just x)) = \y -> check . (*) (signum x * signum y) . read $ (show (abs y) ++ show (abs x))
press (Recall Nothing) = const Nothing
press Store = check
press Inv10 = \x -> check . (*) (signum x) . read . concatMap (\y -> show $ (10 - read [y]) `mod` 10) . show . abs $ x

main :: IO ()
main =
    -- REPL
    forever . runMaybeT $
        ( do
            liftIO . putStrLn . color Blue . replicate 50 $ '-'

            -- turn off buffering
            liftIO $ hSetBuffering stdout NoBuffering

            -- get input values, bind to MaybeT, can fail to alternative
            liftIO . putStr . color Red $ "Start\t\t: "
            Just s <- liftIO getLine <&> readMaybe :: MaybeT IO (Maybe Int)

            liftIO . putStr . color Yellow $ "Goal\t\t: "
            Just g <- liftIO getLine <&> readMaybe :: MaybeT IO (Maybe Int)

            liftIO . putStr . color Cyan $ "Moves\t\t: "
            Just n <- liftIO getLine <&> readMaybe :: MaybeT IO (Maybe Int)

            liftIO . putStr . color Magenta $ "Buttons\t\t: "
            Just bs <- liftIO getLine <&> readMaybe :: MaybeT IO (Maybe [Button])

            liftIO . putStr . color Blue $ "Portals\t\t: "
            ps <- liftIO getLine <&> readMaybe :: MaybeT IO (Maybe (Int, Int))

            -- add virtual "Recall" button if Store is present
            let bs' = if Store `elem` bs then bs ++ [Recall Nothing] else bs

            -- evaluate all possible combinations of buttons
            let y =
                    find (const True)
                        . map (\(_, (x, _)) -> map show x)
                        . filter (\(_, (_, y)) -> y == Just g)
                        . map (\y -> runState (eval y ps) (bs', Just s))
                        . cpn [0 .. length bs' - 1]
                        $ n

            -- print solution
            liftIO . putStrLn . color Green . style Bold . (++) "Solution\t: " . maybe (color Red "No Solution") (color Green . intercalate " | ") $ y
        )
            -- alternative to maybe, error has occured
            <|> liftIO
                ( do
                    putStrLn . style Bold . color Red $ "Error"
                    putStrLn ""
                )