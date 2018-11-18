data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit a) = a
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)

printExpr :: Expr -> String
printExpr (Lit a) = show a
printExpr (Add expr1 expr2) = (printExpr expr1) ++ " + " ++ (printExpr expr2)