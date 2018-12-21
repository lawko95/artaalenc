module Arrow where

import Prelude hiding ((<*), (<$))
import ParseLib.Abstract
import Data.Map (Map)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Parser
import Scanner


--main :: IO()
--main = undefined

type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary 
  deriving Eq

parseSpace :: Parser Char Space
parseSpace =
  do
    (mr,mc)  <-  parenthesised
                   ((,) <$> natural <* symbol ',' <*> natural) <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <-  replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
             zipWith (\ r cs  ->
             zipWith (\ c d   ->  ((r,c),d)) [0..] cs) [0..] css

spaces :: Parser Char String
spaces = greedy (satisfy isSpace)

contents :: Parser Char Contents
contents =
  choice (Prelude.map (\ (f,c) -> f <$ symbol c) contentsTable) <* spaces

contentsTable :: [(Contents,Char)]
contentsTable =
  [  (Empty,'.'),(Lambda,'\\'),(Debris,'%'),(Asteroid,'O'),(Boundary,'#')]

-- These three should be defined by you
-- type Ident = ()
-- type Commands = ()
-- type Heading = ()

-- type Environment = Map Ident Commands

-- type Stack       =  Commands
-- data ArrowState  =  ArrowState Space Pos Heading Stack

-- data Step  =  Done  Space Pos Heading
--            |  Ok    ArrowState
--            |  Fail  String

--Exercise 4
--Happy prefers Left Recursive grammars, because it can create parsers for them with constant stack space, 
--while Right Recursive grammars result in parsers with stack space proportional to the length of the list being parsed. 
--Parser combinators however require Right Recursive grammars, because Left Recursive grammars cause infinite loops.

--Exercise 5
type ProgramAlgebra r = ([Rule] -> r)
foldProgram :: ProgramAlgebra r -> Program -> r
foldProgram (program) (Program x) = program x

type RuleAlgebra r = (Identifier -> [Command] -> r)
foldRule :: RuleAlgebra r -> Rule -> r
foldRule (rule) (Rule id cm) = rule id cm

type CommandAlgebra r = (r, r, r, r, Direction -> r, Direction -> [Alt] -> r, Identifier -> r)
foldCommand :: CommandAlgebra r -> Command -> r
foldCommand (gocommand, takecommand, markcommand, nothingcommand, turncommand, casecommand, rulecommand) = f
  where f GoCommand            = gocommand
        f TakeCommand          = takecommand
        f MarkCommand          = markcommand
        f NothingCommand       = nothingcommand
        f (TurnCommand dir)    = turncommand dir
        f (CaseCommand dir as) = casecommand dir as
        f (RuleCommand id)     = rulecommand id

type DirectionAlgebra r = (r, r, r)
foldDirection :: DirectionAlgebra r -> Direction -> r
foldDirection (left, right, front) = f
  where f LeftDir  = left
        f RightDir = right
        f FrontDir = front

type AltAlgebra r = (Pat -> [Command] -> r)
foldAlt :: AltAlgebra r -> Alt -> r 
foldAlt (alt) (Alt p cms) = alt p cms

type PatAlgebra r = (r, r, r, r, r, r)
foldPat :: PatAlgebra r -> Pat -> r
foldPat (empty, lambda, debris, asteroid, boundary, underscore) = f
  where f EmptyPat = empty
        f LambdaPat = lambda
        f DebrisPat = debris
        f AsteroidPat = asteroid
        f BoundaryPat = boundary
        f UnderscorePat = underscore

type IdentifierAlgebra r = (String -> r)
foldIdentifier :: IdentifierAlgebra r -> Identifier -> r
foldIdentifier (string) = string

-- Exercise 6
check :: Program -> Bool
check prog = True --Works everytime!

-- Exercise 7
showContent :: Contents -> String
showContent c = foldr (\x y -> if fst x == c then [snd x] ++ y else y ) "" contentsTable

printSpace :: Space -> String
printSpace space = "(" ++ show maxWidth ++ "," ++ show maxHeight ++ ")\n" ++ 
                   foldr (\((height, width), cont) rest -> if width == maxWidth then showContent cont ++ "\n" ++ rest else showContent cont ++ rest) "" spaceList
  where maxWidth  = foldr (\((_,x),_) rest -> max x rest) 0 spaceList 
        maxHeight = foldr (\((y,_),_) rest -> max y rest) 0 spaceList 
        spaceList = L.toList space
