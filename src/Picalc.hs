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
          | Send String (Msg a) (Pi a) 
          | Recv String String (Pi a) 
          | New String (Pi a) 
          | Par (Pi a) (Pi a)
          | Peek (Msg a)
          | Bang Int (Pi a)
          | Let String Expr (Pi a)
          | If Expr (Pi a) (Pi a)
    deriving Show
  
type Env a = [(String, Msg a)]

data Value = VInt Integer | VBool Bool deriving (Show, Eq)

data Expr = KonstI Integer
          | KonstB Bool
          | EVar String
          | Add Expr Expr
          | Lt  Expr Expr
          | Le  Expr Expr
          | Eq  Expr Expr
    deriving Show

evalExpr :: Env Value -> Expr -> Value
evalExpr _ (KonstI n) = VInt n
evalExpr _ (KonstB b) = VBool b
evalExpr env (EVar x) =
    case resolveMsg (Var x) env of
        Const v -> v
        _ -> error $ "Expr: " ++ x ++ " is not a value"
evalExpr env (Add e1 e2) =
    case (evalExpr env e1, evalExpr env e2) of
        (VInt x, VInt y) -> VInt (x + y)
        _ -> error "Expr: type error in Add"
evalExpr env (Lt e1 e2) =
    case (evalExpr env e1, evalExpr env e2) of
        (VInt x, VInt y) -> VBool (x < y)
        _ -> error "Expr: type error in Lt"
evalExpr env (Le e1 e2) =
    case (evalExpr env e1, evalExpr env e2) of
        (VInt x, VInt y) -> VBool (x <= y)
        _ -> error "Expr: type error in Le"
evalExpr env (Eq e1 e2) =
    case (evalExpr env e1, evalExpr env e2) of
        (VInt x, VInt y) -> VBool (x == y)
        (VBool x, VBool y) -> VBool (x == y)
        _ -> error "Expr: type error in Eq"

exprVars :: Expr -> [String]
exprVars (EVar x) = [x]
exprVars (Add a b) = exprVars a ++ exprVars b
exprVars (Lt a b) = exprVars a ++ exprVars b
exprVars (Le a b) = exprVars a ++ exprVars b
exprVars (Eq a b) = exprVars a ++ exprVars b
exprVars _ = []

renameExpr :: String -> String -> Expr -> Expr
renameExpr m n (EVar x) = EVar (swap m n x)
renameExpr m n (Add a b) = Add (renameExpr m n a) (renameExpr m n b)
renameExpr m n (Lt a b) = Lt (renameExpr m n a) (renameExpr m n b)
renameExpr m n (Le a b) = Le (renameExpr m n a) (renameExpr m n b)
renameExpr m n (Eq a b) = Eq (renameExpr m n a) (renameExpr m n b)
renameExpr _ _ e = e

data Term a = Term (Pi a) (Env a) deriving Show

freeVars :: Pi a -> [String]
freeVars = nub . freeVars'
    where
        freeVars' (Send x (Var y) p) = [x] ++ [y] ++ freeVars' p 
        freeVars' (Send x _ p) = [x] ++ freeVars' p 
        freeVars' (Recv x y p) = x : filter (/= y) (freeVars' p)
        freeVars' (Par p q) = freeVars' p ++ freeVars' q
        freeVars' (New x p) = filter (/= x) (freeVars' p) 
        freeVars' (Peek (Var x)) = [x]
        freeVars' (Bang _ p) = freeVars' p
        freeVars' (Let x e p) = exprVars e ++ filter (/= x) (freeVars' p)
        freeVars' (If e p q) = exprVars e ++ freeVars' p ++ freeVars' q
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
 
renameMsg :: String -> String -> Msg a -> Msg a
renameMsg m n (Var y) = Var (swap m n y)
renameMsg _ _ msg = msg

renameFree :: String -> String -> Pi a -> Pi a
renameFree _ _ Zero = Zero
renameFree m n (Send x msg p) = Send (swap m n x) (renameMsg m n msg) (renameFree m n p)
renameFree m n (Peek msg) = Peek (renameMsg m n msg)
renameFree m n (Par p q) = Par (renameFree m n p) (renameFree m n q)
renameFree m n (Bang k p) = Bang k (renameFree m n p)
renameFree m n (New x p)
    | x == m = New x p
    | otherwise = New x (renameFree m n p)
renameFree m n (Recv x y p)
    | y == m = Recv (swap m n x) y p
    | otherwise = Recv (swap m n x) y (renameFree m n p)

allNames :: Pi a -> [String]
allNames = nub . go
    where
        go Zero = []
        go (Send x msg p) = x : msgNames msg ++ go p
        go (Recv x y p) = x : y : go p
        go (New x p) = x : go p
        go (Par p q) = go p ++ go q
        go (Peek msg) = msgNames msg
        go (Bang _ p) = go p
        msgNames (Var y) = [y]
        msgNames _ = []

freshName :: String -> [String] -> String
freshName base used = pick candidates
    where
        candidates = (base ++ "_n") : [base ++ "_n" ++ show k | k <- [1 :: Int ..]]
        pick (c:cs) | notElem c used = c
                    | otherwise = pick cs
        pick [] = error "freshName: no candidate"

-- (v a). (v c). par(a(x).c<x>.0 | par((v b). a<b>. 0 | c(z). z))
scopeExt :: Pi a -> Pi a
scopeExt = scopeExtAvoid []

scopeExtAvoid :: [String] -> Pi a -> Pi a
scopeExtAvoid extra t@(Par (New vx p) q)
    | notElem vx (freeVars q) = New vx (Par p q)
    | otherwise =
        let nv = freshName vx (allNames t ++ extra)
        in New nv (Par (renameFree vx nv p) q)
scopeExtAvoid extra t@(Par p (New vx q))
    | notElem vx (freeVars p) = New vx (Par p q)
    | otherwise =
        let nv = freshName vx (allNames t ++ extra)
        in New nv (Par p (renameFree vx nv q))
scopeExtAvoid _ x = x

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
alphaRename m n (Send x (Var y) p) = Send (swap m n x) (Var $ swap m n y) (alphaRename m n p)
alphaRename m n (Send x y p) = Send (swap m n x) y (alphaRename m n p)
alphaRename m n (Recv x y p) = Recv (swap m n x) (swap m n y) (alphaRename m n p)
alphaRename m n (New  x p) = New (swap m n x) (alphaRename m n p) 
alphaRename m n (Par p1 p2) = Par (alphaRename m n p1) (alphaRename m n p2)
alphaRename m n (Peek (Var x)) = Peek (Var $ swap m n x)
alphaRename m n (Bang k p) = Bang k (alphaRename m n p)
alphaRename m n (Let x e p) = Let (swap m n x) (renameExpr m n e) (alphaRename m n p)
alphaRename m n (If e p q) = If (renameExpr m n e) (alphaRename m n p) (alphaRename m n q)
alphaRename _ _ x = x
 
peekTerm :: Msg a -> Env a -> Msg a 
peekTerm (Var x) env = resolveMsg (Var x) env
peekTerm x _ = x

forkPar :: Pi Value -> Env Value -> MVar (Pi Value) -> IO ThreadId
forkPar p env mvar = forkIO $ do
        pt <- (eval $ Term p env) 
        putMVar mvar pt    
    
eval :: Term Value -> IO (Pi Value) 
eval (Term (New vx p) env) = do
    ne <- newTerm vx env 
    np <- eval (Term p ne) 
    return (New vx np)
eval (Term (Send x a p) env) = sendTerm x a env >> eval (Term p env)
eval (Term (Recv x y p) env) =
    recvTerm x y env >>= \ne -> eval (Term p ne)
eval (Term t@(Par (New _ _) _) env) = eval (Term (scopeExtAvoid (map fst env) t) env)
eval (Term t@(Par _ (New _ _)) env) = eval (Term (scopeExtAvoid (map fst env) t) env)
eval (Term (Bang 0 p) env) = pure Zero
eval (Term (Bang k p) env) = eval (Term (Par p (Bang (k-1) p)) env)
eval (Term (Par p1 p2) env) = do
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar
    forkPar p1 env mvar1
    forkPar p2 env mvar2
    pure Par <*> (takeMVar mvar1) <*> (takeMVar mvar2) 
eval (Term (Peek m) env) = return (Peek (peekTerm m env))
eval (Term (Let x e p) env) = eval (Term p ((x, Const (evalExpr env e)) : env))
eval (Term (If e p q) env) =
    case evalExpr env e of
        VBool True -> eval (Term p env)
        VBool False -> eval (Term q env)
        _ -> error "If: the condition is not a boolean"
eval (Term x _) = return x

ex :: IO (Pi Value)
ex = eval (Term (New "c"
    (Par (Send "c" (Const (VInt 7)) Zero)
         (Recv "c" "n"
             (If (Lt (EVar "n") (KonstI 10))
                 (Peek (Const (VBool True)))
                 (Peek (Const (VBool False))))))) [])
