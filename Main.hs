module Main where

import Syntax
import Picalc
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

while :: (Monad m) => MaybeT m b -> m ()
while k = runMaybeT (forever k) >> return ()

main :: IO ()
main = do 
    while $ do
        lift $ putStr "Pika> "
        ln <- lift getLine
        guard $ ln /= ":q"
        e <- lift $ (run ln) :: MaybeT IO (Pi String)
        lift $ putStrLn $ show e
