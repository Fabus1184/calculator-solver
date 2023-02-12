{-# LANGUAGE InstanceSigs #-}

module Calculator (Button (..), eval) where

import Control.Lens (element, (&), (.~))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put)
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.List.Extra ((!?))
import Data.Text (pack, replace, unpack)

data Button
    = Plus Int
    | Minus Int
    | Mult Int
    | -- only integer division allowed
      Div Int
    | Rev
    | -- shift left
      Shl
    | -- append n to the back
      N Int
    | Replace String String
    | -- digital sum
      Sum
    | Pow Int
    | -- rotate right
      RotR
    | -- rotate left
      RotL
    | Mirror
    | -- this should never occur
      Error
    | -- add to every other buttons value
      MetaPlus Int
    | -- store functionality is split into two buttons, recall will be automatically added in main
      Store
    | Recall Int
    | -- digital inverse with 10
      Inv10
    deriving (Eq)

instance Show Button where
    show :: Button -> String
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
    show (Recall x) = "_" ++ show x ++ "_"
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
applyMeta f (Recall x) = Recall $ f x
applyMeta _ x = x

-- evaluate button press
eval :: Int -> Maybe (Int, Int) -> StateT ([Button], Int) Maybe Button
eval b ps = do
    -- get state
    (gr, acc) <- get

    -- mutate state
    acc' <- lift $ press (gr !! b) acc >>= portals ps
    put (gr, acc')

    -- update grid (meta or store buttons)
    updateGrid $ gr !! b

    pure $ gr !! b
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

    updateGrid :: Button -> StateT ([Button], Int) Maybe ()
    -- apply store to each button
    updateGrid Store = do
        (x, y) <- get
        put (map (store y) x, y)
    -- apply meta function to each button
    updateGrid (MetaPlus x) = do
        (gr, acc) <- get
        let gr' = map (applyMeta (+ x)) gr
        put (gr', acc)
    updateGrid x = pure ()

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
press (Recall x) = \y -> check . (*) (signum x * signum y) . read $ (show (abs y) ++ show (abs x))
press Store = check
press Inv10 = \x -> check . (*) (signum x) . read . concatMap (\y -> show $ (10 - read [y]) `mod` 10) . show . abs $ x
