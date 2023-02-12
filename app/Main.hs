module Main (main) where

import Calculator (Button (..), eval, parseButtons)
import Control.Applicative ((<|>))
import Control.Monad (forever, join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (hoistMaybe, runMaybeT, MaybeT)
import Control.Monad.Trans.State (runState, runStateT, evalStateT)
import Data.Functor ((<&>))
import Data.List (find, intercalate)
import System.Console.Pretty (Color (..), Style (..), color, style)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Read (readMaybe)
import Data.Maybe (isJust)

-- all possible arrangements of elements x with length n
cpn :: [a] -> Int -> [[a]]
cpn _ 0 = []
cpn x 1 = map (: []) x
cpn x 2 = [[a, b] | a <- x, b <- x]
cpn x n = [a : b | a <- x, b <- cpn x (n - 1)]

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
            s <- liftIO getLine >>= hoistMaybe . readMaybe

            liftIO . putStr . color Yellow $ "Goal\t\t: "
            g <- liftIO getLine >>= hoistMaybe . readMaybe :: MaybeT IO Int

            liftIO . putStr . color Cyan $ "Moves\t\t: "
            n <- liftIO getLine >>= hoistMaybe . readMaybe

            liftIO . putStr . color Magenta $ "Buttons\t\t: "
            bs <- liftIO getLine >>= hoistMaybe . parseButtons

            liftIO . putStr . color Blue $ "Portals\t\t: "
            ps <- liftIO getLine <&> readMaybe

            -- evaluate all possible combinations of buttons
            let y = 
                        map (\z -> map show <$> evalStateT (eval z ps) (bs, s))
                        . cpn [0 .. length bs - 1]
                        $ n

            -- possible solutions
            liftIO . putStrLn . color Green $ "Number of possible combinations: " ++ show (length bs ^ n)

            liftIO $ mapM_ print y

            -- print solution
            liftIO
                . putStrLn
                . color Green
                . style Bold
                . (++) "Solution\t: "
                . maybe (color Red "No Solution") (color Green . intercalate " | ")
                $ join $ find isJust y
        )
            -- alternative to maybe, error has occured
            <|> liftIO
                ( do
                    putStrLn . style Bold . color Red $ "Error"
                    putStrLn ""
                )
