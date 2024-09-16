module Eval3
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
lookfor :: Variable -> State -> String -> Either (Error, String) (Int, String)
lookfor x s t = case M.lookup x s of
              (Just a)-> Right (a, t) 
              Nothing -> Left (UndefVar, t)

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update x n s = M.adjust (\i->n) x s

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either (Error, String) (State, String)
eval p = stepCommStar p initState ""

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> String -> Either (Error,String) (State,String)
stepCommStar Skip s t = return (s,t)
stepCommStar c    s t = do
  ((c' :!: s'), t') <- stepComm c s t
  stepCommStar c' s' t'


-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> String -> Either (Error, String) ((Pair Comm State), String)
stepComm Skip s t         = Right ((Skip :!: s), t)  
stepComm (Let x e) s t     = do 
                      ((n :!: s'), t') <- evalExp e s t
                      return ((Skip :!: M.insert x n s'), t' ++ "Let " ++ x ++ " = " ++ show n ++ "\n")
stepComm (Seq Skip c) s t  = Right ((c :!: s), t) 
stepComm (Seq c0 c1) s t  = do 
                        ((c0' :!: s'), t') <- stepComm c0 s t
                        return (((Seq c0' c1) :!: s'), t')
stepComm (IfThenElse b c0 c1) s t = do
                                 ((b' :!: s'), t') <- evalExp b s t 
                                 if b' then return ((c0 :!: s'), t')
                                 else return ((c1 :!: s'), t')
stepComm (RepeatUntil c b) s t = do
                            ((c1 :!: s'), t') <- stepComm (IfThenElse b Skip (RepeatUntil c b)) s t
                            return (((Seq c c1) :!: s'), t')

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> String ->Either (Error, String) ((Pair a State), String)
evalExp (Const n)  s t = evalExpInt (Const n) s t
evalExp (Var x)    s t = evalExpInt (Var x) s t
evalExp (VarInc x) s t = evalExpInt (VarInc x) s t
evalExp (VarDec x) s t = evalExpInt (VarDec x) s t
evalExp (UMinus e) s t = evalExpInt (UMinus e) s t
evalExp (Plus e0 e1)  s t = evalExpInt (Plus e0 e1)  s t
evalExp (Minus e0 e1) s t = evalExpInt (Minus e0 e1) s t
evalExp (Times e0 e1) s t = evalExpInt (Times e0 e1) s t
evalExp (Div e0 e1)   s t = evalExpInt (Div e0 e1)   s t 
evalExp BTrue       s t = evalExpBool BTrue s t
evalExp BFalse      s t = evalExpBool BFalse s t
evalExp (Not p)     s t = evalExpBool (Not p) s t
evalExp (And p0 p1) s t = evalExpBool (And p0 p1) s t  
evalExp (Or p0 p1)  s t = evalExpBool (Or p0 p1)  s t 
evalExp (Lt e0 e1)  s t = evalExpBool (Lt e0 e1)  s t
evalExp (Gt e0 e1)  s t = evalExpBool (Gt e0 e1)  s t
evalExp (Eq e0 e1)  s t = evalExpBool (Eq e0 e1)  s t
evalExp (NEq e0 e1) s t = evalExpBool (NEq e0 e1) s t 



evalExpInt :: Exp Int -> State -> String ->Either (Error, String) ((Pair Int State), String)
evalExpInt (Const n)  s t = Right ((n :!: s), t)
evalExpInt (Var x)    s t = do
                      (x', st) <- lookfor x s t
                      return ((x' :!: s), t)
evalExpInt (VarInc x) s t =  do 
                           (n, st) <- lookfor x s t
                           let s' = update x (n + 1) s 
                           return (((n+1) :!: s'), t ++ "Let " ++ x ++ " = " ++ show (n+1) ++ "\n")   
evalExpInt (VarDec x) s t = do 
                           (n, st) <- lookfor x s t
                           let s' = update x (n - 1) s 
                           return (((n-1) :!: s'), t ++ "Let " ++ x ++ " = " ++ show (n+1) ++ "\n") 
evalExpInt (UMinus e) s t = do
                      ((n :!: s'), t') <- evalExp e s t
                      return ((-n :!: s'), t')
evalExpInt expr s t = do evalOpp expr s t

evalExpBool::Exp Bool -> State -> String -> Either (Error, String) ((Pair Bool State), String)
evalExpBool BTrue  s t = Right ((True  :!: s), t)
evalExpBool BFalse s t = Right ((False :!: s), t)
evalExpBool (Not p) s t = case evalExpBool p s t of
                         Left (DivByZero, t')       -> Left (DivByZero, t')
                         Left (UndefVar, t')        -> Left (UndefVar, t')
                         Right ((True :!: s'), t') -> Right ((False :!: s'), t')
                         Right ((False :!: s'), t') -> Right ((True :!: s'), t')
evalExpBool (And p0 p1) s t = evalLog (&&) p0 p1 s t
evalExpBool (Or p0 p1)  s t = evalLog (||) p0 p1 s t
evalExpBool (Lt e0 e1)  s t = evalComp (<)  e0 e1 s t
evalExpBool (Gt e0 e1)  s t = evalComp (>)  e0 e1 s t
evalExpBool (Eq e0 e1)  s t = evalComp (==) e0 e1 s t
evalExpBool (NEq e0 e1) s t = evalComp (/=) e0 e1 s t


evalOpp :: Exp Int -> State -> String ->Either (Error, String) ((Pair Int State), String)
evalOpp (Div x y) s t = 
  do
    ((n0 :!: s'), t') <- evalExpInt x s t
    ((n1 :!: s''), t'') <- evalExpInt y s' t'
    if n1 == 0 then Left (DivByZero, t'')
    else return ((div n0 n1 :!: s''), t'')
evalOpp expri s t =
  let (op, x, y) = getOp expri in
  do
    ((n0 :!: s'), t')<- evalExpInt x s t
    ((n1 :!: s''), t'') <- evalExpInt y s' t'
    return ((op n0 n1 :!: s''), t'')

getOp :: Exp Int -> (Int -> Int -> Int,Exp Int,Exp Int)
getOp (Times x y) =  ((*),x,y)
getOp (Minus x y) = ((-),x,y)
getOp (Plus x y) = ((+),x,y)

evalLog :: (Bool -> Bool -> Bool) -> Exp Bool -> Exp Bool -> State -> String -> Either (Error,String) ((Pair Bool State), String)
evalLog op p0 p1 s t = do 
                  ((n0 :!: s'), t') <- evalExpBool p0 s t
                  ((n1 :!: s''), t'') <- evalExpBool p1 s' t'
                  return ((op n0 n1 :!: s''),t'')


evalComp :: (Int -> Int -> Bool) -> Exp Int -> Exp Int -> State -> String-> Either (Error,String) ((Pair Bool State),String)
evalComp op e0 e1 s t = do 
                    ((n0 :!: s'), t')  <- evalExpInt e0 s t
                    ((n1 :!: s''), t'') <- evalExpInt e1 s' t'
                    return ((op n0 n1 :!: s''), t'')