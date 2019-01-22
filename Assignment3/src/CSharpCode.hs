module CSharpCode where

import Prelude hiding (LT, GT, EQ, (<*),(*>),(<$))
import Data.Map as M
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import Data.Char


data ValueOrAddress = Value | Address
    deriving Show

type Environment = Map String Int 

codeAlgebra :: CSharpAlgebra Code Code (Environment -> Code) (ValueOrAddress -> Environment -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprMethPar)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> (Environment -> Code) -> Code
fMembMeth t (LowerId x) ps s = [LABEL x] ++ s env ++ [RET]
    where env                           =  fromList(pairMaker)
          pairMaker                     =  zip (pairKeys)(pairValues)
          pairValues                    = [-length ps..]
          pairKeys                      = Prelude.map getKey ps 
          getKey (Decl _ (LowerId key)) = key
          
-- zip
--[-7..]
--Decl Token (LowerId x) -> x
--x -> ((x,-(length ps))
---7 -6 ... -1
--  -7  [x, -7]
--fromList [(x, -7), (y, -6)] -- maakt environment
--(-10, -9)
--zip (ps)

fStatDecl :: Decl -> Environment -> Code
fStatDecl env d = []

fStatExpr :: (ValueOrAddress -> Environment -> Code) -> Environment -> Code
fStatExpr e env = e Value env ++ [pop]

fStatIf :: (ValueOrAddress -> Environment -> Code) -> (Environment -> Code) -> (Environment -> Code) -> Environment -> Code
fStatIf e s1 s2 env = c ++ [BRF (n1 + 2)] ++ s1 env ++ [BRA n2] ++ s2 env
                where c        = e Value env 
                      (n1, n2) = (codeSize (s1 env), codeSize (s2 env))

fStatWhile :: (ValueOrAddress -> Environment -> Code) -> (Environment -> Code) -> Environment -> Code
fStatWhile e s1 env = [BRA n] ++ s1 env ++ c ++ [BRT (-(n + k + 2))]
                where  c      = e Value env
                       (n, k) = (codeSize (s1 env), codeSize c)    

fStatReturn :: (ValueOrAddress -> Environment -> Code) -> Environment -> Code
fStatReturn e env = e Value env ++ [pop] ++ [RET]

fStatBlock :: [Environment -> Code] -> Environment -> Code
fStatBlock cod env = concatMap (\e -> e env) cod 

fExprCon :: Token -> ValueOrAddress -> Environment -> Code
fExprCon (ConstInt n) _ _     = [LDC n]
fExprCon (ConstChar c) _ _    = [LDC (ord c)]
fExprCon (ConstBool True) _ _ = [LDC 1]
fExprCon (ConstBool _) _ _    = [LDC 0]

fExprVar :: Token -> ValueOrAddress -> Environment -> Code
fExprVar (LowerId x) va env = let loc = env ! x in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

-- Task 7
fExprOp :: Token -> (ValueOrAddress -> Environment -> Code) -> (ValueOrAddress -> Environment -> Code) -> ValueOrAddress -> Environment -> Code
fExprOp (Operator "=") e1 e2 va env  = e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
fExprOp (Operator "&&") e1 e2 va env = e1 Value env ++ e1 Value env ++ [BRF (codeSize code)] ++ code
                          where code = e2 Value env ++ [AND]
fExprOp (Operator "||") e1 e2 va env = e1 Value env ++ e1 Value env ++ [BRT (codeSize code)] ++ code
                          where code = e2 Value env ++ [OR]
--fExprOp (Operator "||") e1 e2 va = e1 Value ++ [BRT iets] ++ e2 Value
fExprOp (Operator op)   e1 e2 va env = e1 Value env ++ e2 Value env ++ [opCodes ! op]

-- Tasks 6 and 8
fExprMethPar :: Token -> [ValueOrAddress -> Environment -> Code] -> ValueOrAddress -> Environment -> Code
fExprMethPar (LowerId "print") exprs va env = concat (Prelude.map (\e -> e va env ++ [TRAP 0]) exprs)
fExprMethPar (LowerId x) exprs va env = concat (Prelude.map (\e -> e va env) exprs) ++ [Bsr x]


opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]


