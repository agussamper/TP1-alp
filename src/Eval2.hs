module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor x s = case M.lookup x s of
              (Just a)-> Right a 
              Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update x n s = M.adjust (\i->n) x s

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'


-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s          = Right (Skip :!: s)  
stepComm (Let x e) s     = do 
                      (n :!: s') <- evalExp e s 
                      return (Skip :!: M.insert x n s')
stepComm (Seq Skip c) s  = Right (c :!: s) 
stepComm (Seq c0 c1) s   = do 
                        (c0' :!: s') <- stepComm c0 s
                        return ((Seq c0' c1) :!: s')
stepComm (IfThenElse b c0 c1) s = do
                                (b' :!: s') <- evalExp b s 
                                if b' then return (c0 :!: s')
                                else return (c1 :!: s')
stepComm (RepeatUntil c b) s = do
                            (c1 :!: s')<- stepComm (IfThenElse b Skip (RepeatUntil c b)) s
                            return ((Seq c c1) :!: s')

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n)  s = evalExpInt (Const n) s
evalExp (Var x)    s = evalExpInt (Var x) s
evalExp (VarInc x) s = evalExpInt (VarInc x) s
evalExp (VarDec x) s = evalExpInt (VarDec x) s
evalExp (UMinus e) s = evalExpInt (UMinus e) s
evalExp (Plus e0 e1)  s = evalExpInt (Plus e0 e1)  s
evalExp (Minus e0 e1) s = evalExpInt (Minus e0 e1) s
evalExp (Times e0 e1) s = evalExpInt (Times e0 e1) s
evalExp (Div e0 e1)   s = evalExpInt (Div e0 e1)   s
evalExp BTrue       s = evalExpBool BTrue s
evalExp BFalse      s = evalExpBool BFalse s
evalExp (Not p)     s = evalExpBool (Not p) s
evalExp (And p0 p1) s = evalExpBool (And p0 p1) s  
evalExp (Or p0 p1)  s = evalExpBool (Or p0 p1)  s 
evalExp (Lt e0 e1)  s = evalExpBool (Lt e0 e1)  s
evalExp (Gt e0 e1)  s = evalExpBool (Gt e0 e1)  s
evalExp (Eq e0 e1)  s = evalExpBool (Eq e0 e1)  s
evalExp (NEq e0 e1) s = evalExpBool (NEq e0 e1) s



evalExpInt :: Exp Int -> State -> Either Error (Pair Int State)
evalExpInt (Const n)  s = Right (n :!: s)
evalExpInt (Var x)    s = do
                      x' <- lookfor x s 
                      return (x' :!: s)
evalExpInt (VarInc x) s =  do 
                           n <- lookfor x s
                           let s' = update x (n + 1) s 
                           return ((n+1) :!: s')
evalExpInt (VarDec x) s = do 
                           n <- lookfor x s
                           let s' = update x (n - 1) s 
                           return ((n-1) :!: s')
evalExpInt (UMinus e) s = do
                      (n :!: s') <- evalExp e s
                      return (-n :!: s')
evalExpInt expr s = do evalOpp expr s

evalExpBool::Exp Bool -> State -> Either Error (Pair Bool State)
evalExpBool BTrue  s = Right (True  :!: s)
evalExpBool BFalse s = Right (False :!: s)
evalExpBool (Not p) s = case evalExpBool p s of
                         Left DivByZero       -> Left DivByZero
                         Left UndefVar        -> Left UndefVar
                         Right (True :!: s')  -> Right (False :!: s')
                         Right (False :!: s') -> Right (True :!: s')
evalExpBool (And p0 p1) s = evalLog (&&) p0 p1 s
evalExpBool (Or p0 p1)  s = evalLog (||) p0 p1 s
evalExpBool (Lt e0 e1)  s = evalComp (<)  e0 e1  s
evalExpBool (Gt e0 e1)  s = evalComp (>)  e0 e1  s
evalExpBool (Eq e0 e1)  s = evalComp (==) e0 e1 s
evalExpBool (NEq e0 e1) s = evalComp (/=) e0 e1 s


evalOpp :: Exp Int -> State -> Either Error (Pair Int State)
evalOpp (Div x y) s = 
  do
    (n0 :!: s')  <- evalExpInt x s
    (n1 :!: s'') <- evalExpInt y s'
    if n1 == 0 then Left DivByZero
    else return (div n0 n1 :!: s'')
evalOpp expri s =
  let (op, x, y) = getOp expri in
  do
    (n0 :!: s') <- evalExpInt x s
    (n1 :!: s'') <- evalExpInt y s'
    return (op n0 n1 :!: s'')

getOp :: Exp Int -> (Int -> Int -> Int,Exp Int,Exp Int)
getOp (Times x y) =  ((*),x,y)
getOp (Minus x y) = ((-),x,y)
getOp (Plus x y) = ((+),x,y)

evalLog :: (Bool -> Bool -> Bool) -> Exp Bool -> Exp Bool -> State -> Either Error (Pair Bool State)
evalLog op p0 p1 s = do 
                  (n0 :!: s')  <- evalExpBool p0 s
                  (n1 :!: s'') <- evalExpBool p1 s'
                  return (op n0 n1 :!: s'')


evalComp :: (Int -> Int -> Bool) -> Exp Int -> Exp Int -> State -> Either Error (Pair Bool State)
evalComp op e0 e1 s = do 
                    (n0 :!: s')  <- evalExpInt e0 s
                    (n1 :!: s'') <- evalExpInt e1 s'
                    return (op n0 n1 :!: s'')