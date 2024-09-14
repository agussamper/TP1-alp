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
lookfor w ((x, n) : s) | (w == x) = n
                       | otherwise = lookfor w s

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update w n ((x, y) : s) | (w == x) = (x, n) : s
                        | otherwise = (x, y) : (update w n s)

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
stepComm Skip s          = (Skip !:! s)  
stepComm (Let x n) s     = (Skip !:! update x n s)
stepComm (Seq Skip c) s  = (c2 !:! s) 
stepComm (Seq c0 c1) s   = ((Seq c0' c1) !:! s')
                        where
                          (c0' !:! s') = stepComm c0 s
stepComm (IfThenElse b c0 c1) s = case evalExp b s of
                                      (True !:! s')  -> (c0 !:! s')
                                      (False !:! s') -> (c1 !:! s')
stepComm RepeatUntil c b = (Seq c (IfThenElse b Skip (RepeatUntil c b)) !:! s)

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = (n !:! s)
evalExp (Var x) s = (lookfor x s !:! s)
evalExp (VarInc x) s = (x+1 !:! update (x+1) s)
evalExp (VarDec x) s = (x-1 !:! update (x-1) s)
evalExp (UMinus e) s = (-n !:! s')
                    where 
                      (n !:! s') = evalExp e s
evalExp expr s = let (n0 !:! s')  = evalExp e0 s
                     (n1 !:! s'') = evalExp e1 s'
                 in case expr of
                     Plus e0 e1  -> (n0 + n1 !:! s'')
                     Minus e0 e1 -> (n0 - n1 !:! s'')
                     Times e0 e1 -> (n0 * n1 !:! s'')
                     Div e0 e1   -> (n0 `div` n1 !:! s'')
evalExp BTrue s = (True !:! s)
evalExp BFalse s = (False !:! s)
evalExp (Not p) s = case evalExp p s of
                         (True !:! s')  -> (False !:! s')
                         (False !:! s') -> (True !:! s')
evalExp expr = let (b0 !:! s')  = evalExp p1 s
                   (b1 !:! s'') = evalExp p0 s'
                 in case expr of
                         And p0 p1 ->  (b0 && b1 !:! s'')  
                         Or p0 p1  ->  (b0 || b1 !:! s'')
evalExp expr s = let (n0' !:! s')  = evalExp e0 s
                     (n1' !:! s'') = evalExp e1 s'
                 in case expr of
                         Lt e0 e1  -> (n0' < n1' !:! s'')
                         Gt e0 e1  -> (n0' > n1' !:! s'')
                         Eq e0 e1  -> (n0' == n1' !:! s'')
                         NEq e0 e1 -> (n0' != n1' !:! s'')



