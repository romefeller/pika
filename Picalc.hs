module Picalc where 

import Control.Concurrent.STM
import Control.Concurrent
import Data.Maybe
import Data.List
import GHC.Conc

data Msg a = Const a | Var String | Channel (Chan (Msg a)) 

instance Show a => Show (Msg a) where 
    show (Channel _) = "Chan"
    show (Const x) = show x
    show (Var x) = x

data Pi a = Zero 
          | Send String (Msg a) (Pi a) 
          | Recv String String (Pi a) 
          | New String (Pi a) 
          | Par (Pi a) (Pi a)
          | Peek (Msg a)
          | Bang Int (Pi a)
    deriving Show
       
type Env a = [(String, Msg a)]

data Term a = Term (Pi a) (Env a) deriving Show

freeVars :: Pi a -> [String]
freeVars = nub . freeVars'
    where
        freeVars' (Send x (Var y) p) = [x] ++ [y] ++ freeVars' p 
        freeVars' (Send x _ p) = [x] ++ freeVars' p 
        freeVars' (Recv x y p) = filter (/= y) ([x] ++ freeVars' p) 
        freeVars' (Par p q) = freeVars' p ++ freeVars' q
        freeVars' (New x p) = filter (/= x) (freeVars' p) 
        freeVars' _ = []
        
newTerm :: String -> Env a -> IO (Env a)
newTerm vx env = do 
    nu <- newChan 
    return (env ++ [(vx, Channel nu)])
 
sendTerm :: String -> Msg a -> Env a -> IO ()
sendTerm x a env = 
    case lookup x env of
            Nothing -> error $ "Send: Channelel " ++ x ++ " not found"
            Just (Var y) -> sendTerm y a env
            Just (Channel chan) -> writeChan chan a
            Just _ -> error $ "Send: illegal Channelel name "
 
recvTerm :: String -> String -> Env a -> IO (Env a)
recvTerm x y env = 
    case lookup x env of
            Nothing -> error $ "Recv: Channelel " ++ x ++ " not found"
            Just (Var z) -> recvTerm z y env
            Just (Channel chan) -> do
                v <- readChan chan
                return (env++[(y,v)])
            Just _ -> error $ "Recv: illegal Channelel name "
 
scopeExt :: Pi a -> Pi a
scopeExt (Par (New vx p) q) 
    | notElem vx (freeVars q) = New vx (Par p q)
    | otherwise = New (vx ++ "_n") (Par p q)
scopeExt (Par p (New vx q)) 
    | notElem vx (freeVars p) = New vx (Par p q)
    | otherwise = New (vx ++ "_n") (Par p q)
scopeExt x = x
 
peekTerm :: Msg a -> Env a -> Msg a 
peekTerm (Var x) env = 
    case lookup x env of
         Nothing -> error $ "Peek: Illegal variable name " ++ x
         Just v -> v
peekTerm x _ = x

forkPar :: Pi a -> Env a -> MVar (Pi a) -> IO ThreadId
forkPar p env mvar = forkIO $ do
        pt <- (eval $ Term p env) 
        putMVar mvar pt    
    
eval :: Term a -> IO (Pi a) 
eval (Term (New vx p) env) = do
    ne <- newTerm vx env 
    np <- eval (Term p ne) 
    return (New vx np)
eval (Term (Send x a p) env) = 
    sendTerm x a env >> eval (Term p env)
eval (Term (Recv x y p) env) =
    recvTerm x y env >>= \ne -> eval (Term p ne)
eval (Term t@(Par (New vx p) q) env) = eval (Term (scopeExt t) env)
eval (Term t@(Par p (New vx q)) env) = eval (Term (scopeExt t) env)
eval (Term (Bang 0 p) env) = eval (Term p env)
eval (Term (Bang k p) env) = eval (Term (Par p (Bang (k-1) p)) env)
eval (Term (Par p1 p2) env) = do
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar
    forkPar p1 env mvar1
    forkPar p2 env mvar2
    pure Par <*> (takeMVar mvar1) <*> (takeMVar mvar2) 
eval (Term (Peek m) env) = return (Peek (peekTerm m env))
eval (Term x _) = return x
