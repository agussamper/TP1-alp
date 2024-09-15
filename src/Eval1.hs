module Eval1
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
lookfor :: Variable -> State -> Int
lookfor x s = case M.lookup x s of
              (Just a)-> a 

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update x n s = M.adjust (\i->n) x s

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s          = (Skip :!: s)  
stepComm (Let x n) s     = let (n' :!: s') = evalExp n s
                           in (Skip :!: update x n' s')
stepComm (Seq Skip c) s  = (c :!: s) 
stepComm (Seq c0 c1) s   = let (c0' :!: s') = stepComm c0 s
                           in ((Seq c0' c1) :!: s')
stepComm (IfThenElse b c0 c1) s = case evalExp b s of
                                      (True :!: s')  -> (c0 :!: s')
                                      (False :!: s') -> (c1 :!: s')
stepComm (RepeatUntil c b) s = (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
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



evalExpInt :: Exp Int -> State -> Pair Int State
evalExpInt (Const n)  s = (n :!: s)
evalExpInt (Var x)    s = (lookfor x s :!: s)
evalExpInt (VarInc x) s = let n = lookfor x s
                          in ((n+1) :!: update x (n+1) s)
evalExpInt (VarDec x) s = let n = lookfor x s
                          in ((n-1) :!: update x (n-1) s)
evalExpInt (UMinus e) s = let (n :!: s') = evalExp e s
                          in (-n :!: s')
evalExpInt (Plus  e0 e1) s = evalOpp (+)   e0 e1 s
evalExpInt (Minus e0 e1) s = evalOpp (-)   e0 e1 s
evalExpInt (Times e0 e1) s = evalOpp (*)   e0 e1 s
evalExpInt (Div   e0 e1) s = evalOpp (div) e0 e1 s

evalExpBool::Exp Bool -> State -> Pair Bool State
evalExpBool BTrue  s = (True  :!: s)
evalExpBool BFalse s = (False :!: s)
evalExpBool (Not p) s = case evalExpBool p s of
                         (True :!: s')  -> (False :!: s')
                         (False :!: s') -> (True :!: s')
evalExpBool (And p0 p1) s = evalLog (&&) p0 p1 s
evalExpBool (Or p0 p1)  s = evalLog (||) p0 p1 s
evalExpBool (Lt e0 e1)  s = evalComp (<)  e0 e1  s
evalExpBool (Gt e0 e1)  s = evalComp (>)  e0 e1  s
evalExpBool (Eq e0 e1)  s = evalComp (==) e0 e1 s
evalExpBool (NEq e0 e1) s = evalComp (/=) e0 e1 s


evalOpp :: (Int -> Int -> Int) -> Exp Int -> Exp Int -> State -> Pair Int State
evalOpp op e0 e1 s = let (n0 :!: s')  = evalExpInt e0 s
                         (n1 :!: s'') = evalExpInt e1 s'
                      in (op n0 n1 :!: s'')


evalLog :: (Bool -> Bool -> Bool) -> Exp Bool -> Exp Bool -> State -> Pair Bool State
evalLog op p0 p1 s = let (n0 :!: s')  = evalExpBool p0 s
                         (n1 :!: s'') = evalExpBool p1 s'
                      in (op n0 n1 :!: s'')


evalComp :: (Int -> Int -> Bool) -> Exp Int -> Exp Int -> State -> Pair Bool State
evalComp op e0 e1 s = let (n0 :!: s')  = evalExpInt e0 s
                          (n1 :!: s'') = evalExpInt e1 s'
                      in (op n0 n1 :!: s'')