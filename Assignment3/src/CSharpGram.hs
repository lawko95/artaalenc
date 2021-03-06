module CSharpGram where

import Prelude hiding ((<*),(*>),(<$))
import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex


data Class = Class Token [Member]
    deriving Show

data Member = MemberD Decl
            | MemberM Type Token [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl 
          | StatExpr   Expr 
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatFor    Expr Expr Stat 
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

-- Task 6
data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
          | ExprMethPar Token [Expr]
          deriving Show

data Decl = Decl Type Token
    deriving Show   

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving (Eq,Show)


parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> pExprMethPar
           <|> parenthesised pExpr
  
-- Task 6
pExprMethPar :: Parser Token Expr
pExprMethPar = ExprMethPar <$> sLowerId <*> parenthesised (option (listOf pExpr (symbol Comma)) [])

-- Task 2
multis :: Parser Token Expr
multis = chainl pExprSimple (ExprOper <$> sOperator7)

addis :: Parser Token Expr
addis = chainl multis (ExprOper <$> sOperator6)

comparis :: Parser Token Expr
comparis = chainl addis (ExprOper <$> sOperator4)

andis :: Parser Token Expr
andis = chainl comparis (ExprOper <$> sOperator3)

oris :: Parser Token Expr
oris = chainl andis (ExprOper <$> sOperator2)

--moet nog voor non-exhaustive maken dus een geval zonder operator ofso isOperator7 _ = error "wtf"

-- Chainr to make the assignment operator right-associative (Task 4)
pExpr :: Parser Token Expr
pExpr = chainr oris (ExprOper <$> sOperator1)

sOperator7 :: Parser Token Token 
sOperator7 = satisfy isOperator7
  where isOperator7 (Operator x) = elem x multis
        isOperator7   _          = error "Illegal expression"
        multis = ["*", "/", "%"]
        
sOperator6 :: Parser Token Token 
sOperator6 = satisfy isOperator6
  where isOperator6 (Operator x) = elem x addis
        isOperator6 _            = error "Illegal expression" 
        addis = ["+","-"]

sOperator4 :: Parser Token Token 
sOperator4 = satisfy isOperator4
  where isOperator4 (Operator x) = elem x comparis
        isOperator4 _            = error "Illegal expression"
        comparis = ["<=", "<", ">=", ">", "==", "!=", "="]

sOperator3 :: Parser Token Token 
sOperator3 = satisfy isOperator3
  where isOperator3 (Operator x) = elem x andis
        isOperator3 _            = error "Illegal expression"
        andis = ["&&"]

sOperator2 :: Parser Token Token 
sOperator2 = satisfy isOperator2
  where isOperator2 (Operator x) = elem x oris
        isOperator2 _            = error "Illegal expression"
        oris = ["||", "^"]

sOperator1 :: Parser Token Token
sOperator1 = satisfy isOperator1
  where isOperator1 (Operator "=") = True
        isOperator1 _              = error "Illegal expression"



pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi


-- Task 5
pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat 
     <|> (\(StatFor d e s) -> StatBlock ((StatExpr d) : [StatWhile e s])) <$> pFor
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
     <|> pBlock
    where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])

-- Task 5
pFor :: Parser Token Stat
pFor = StatFor <$ symbol KeyFor <* symbol POpen <*> pExpr <* sSemi <*> pExpr <* sSemi <*> ((\s ss -> StatBlock (s : [ss])) <$> (StatExpr <$> pExpr) <* symbol PClose <*> pStat)

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)


pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))


pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

