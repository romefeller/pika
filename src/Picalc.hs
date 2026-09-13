module Picalc where 

import Control.Concurrent.STM
import Control.Concurrent
import Data.Maybe
import Data.List
import GHC.Conc

data Msg a = Const a | Var String | Channel (Chan (Msg a, MVar ())) 

instance Show a => Show (Msg a) where 
    show (Channel _) = "Chan"
    show (Const x) = show x
    show (Var x) = x

data Pi a = Zero 
          | Send String (Msg a) 
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
        freeVars' (Send x (Var y)) = [x] ++ [y] 
        freeVars' (Send x _) = [x] 
        freeVars' (Recv x y p) = x : filter (/= y) (freeVars' p)
        freeVars' (Par p q) = freeVars' p ++ freeVars' q
        freeVars' (New x p) = filter (/= x) (freeVars' p) 
        freeVars' (Peek (Var x)) = [x]
        freeVars' (Bang _ p) = freeVars' p
        freeVars' _ = []
        
newTerm :: String -> Env a -> IO (Env a)
newTerm vx env = do 
    nu <- newChan 
    return ((vx, Channel nu) : env)
 
resolveMsg :: Msg a -> Env a -> Msg a
resolveMsg m env = resolve m []
    where
        resolve (Const value) _ = Const value
        resolve (Channel channel) _ = Channel channel
        resolve (Var name) visited
            | elem name visited =
                error $ "Resolve: cyclic alias for " ++ name
            | otherwise =
                case lookup name env of
                    Just stored -> resolve stored (name : visited)
                    Nothing -> error $ "Resolve: name " ++ name ++ " not found"

sendTerm :: String -> Msg a -> Env a -> IO ()
sendTerm x a env = 
    case lookup x env of
            Nothing -> error $ "Send: Channelel " ++ x ++ " not found"
            Just (Var y) -> sendTerm y a env
            Just (Channel chan) -> do 
                ack <- newEmptyMVar
                writeChan chan (resolveMsg a env, ack)
                takeMVar ack
            Just _ -> error $ "Send: illegal Channelel name "
 
recvTerm :: String -> String -> Env a -> IO (Env a)
recvTerm x y env = 
    case lookup x env of
            Nothing -> error $ "Recv: Channelel " ++ x ++ " not found"
            Just (Var z) -> recvTerm z y env
            Just (Channel chan) -> do
                (v, ack) <- readChan chan
                putMVar ack ()
                return ((y, v) : env)
            Just _ -> error $ "Recv: illegal Channelel name "

swap m n x = if x == m then n else x            
            
alphaRename :: String -> String -> Pi a -> Pi a 
alphaRename m n (Send x (Var y)) = Send (swap m n x) (Var $ swap m n y)
alphaRename m n (Send x y) = Send (swap m n x) y 
alphaRename m n (Recv x y p) = Recv (swap m n x) (swap m n y) (alphaRename m n p)
alphaRename m n (New  x p) = New (swap m n x) (alphaRename m n p) 
alphaRename m n (Par p1 p2) = Par (alphaRename m n p1) (alphaRename m n p2)
alphaRename m n (Peek (Var x)) = Peek (Var $ swap m n x)
alphaRename m n (Bang k p) = Bang k (alphaRename m n p)
alphaRename _ _ x = x
         
-- (v a). (v c). par(a(x).c<x>.0 | par((v b). a<b>. 0 | c(z). z))
scopeExt :: Pi a -> Pi a
scopeExt (Par (New vx p) q) 
    | notElem vx (freeVars q) = New vx (Par p q)
    | otherwise = let nv = vx ++ "_n" in New nv (Par (alphaRename vx nv p) q)
scopeExt (Par p (New vx q)) 
    | notElem vx (freeVars p) = New vx (Par p q)
    | otherwise = let nv = vx ++ "_n" in New nv (Par p (alphaRename vx nv q))
scopeExt x = x
 
peekTerm :: Msg a -> Env a -> Msg a 
peekTerm (Var x) env = resolveMsg (Var x) env
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
eval (Term (Send x a) env) = sendTerm x a env >> return Zero
eval (Term (Recv x y p) env) =
    recvTerm x y env >>= \ne -> eval (Term p ne)
eval (Term t@(Par (New vx p) q) env) = eval (Term (scopeExt t) env)
eval (Term t@(Par p (New vx q)) env) = eval (Term (scopeExt t) env)
eval (Term (Bang 0 p) env) = pure Zero
eval (Term (Bang k p) env) = eval (Term (Par p (Bang (k-1) p)) env)
eval (Term (Par p1 p2) env) = do
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar
    forkPar p1 env mvar1
    forkPar p2 env mvar2
    pure Par <*> (takeMVar mvar1) <*> (takeMVar mvar2) 
eval (Term (Peek m) env) = return (Peek (peekTerm m env))
eval (Term x _) = return x
