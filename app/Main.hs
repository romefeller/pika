module Main where

import Syntax
import Picalc
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.IO (hFlush, stdout)

while :: (Monad m) => MaybeT m b -> m ()
while k = runMaybeT (forever k) >> return ()

main :: IO ()
main = do 
    while $ do
        lift $ putStr "Pika> "
        lift $ hFlush stdout
        ln <- lift getLine
        guard $ ln /= ":q"
        e <- lift $ (run ln) :: MaybeT IO (Pi Value)
        lift $ putStrLn $ show e
