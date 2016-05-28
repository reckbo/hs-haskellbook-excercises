import Data.Function

data Expr
   = Lit Integer
   | Add Expr Expr

eval :: Expr -> Integer 
eval (Add e1 e2) = ((+)`on`eval) e1 e2
eval (Lit x) = x

printExpr :: Expr -> String 
printExpr (Add e1 e2) =  s1 ++ " + " ++ s2
                        where s1 = printExpr e1
                              s2 = printExpr e2
printExpr (Lit x) = show x

