{
module Parser where
import Scanner
}

%name parseCalc
%tokentype { Token }
%error { parseError }



%token
    PArrow { TArrow }
    PDot  { TDot }
    PComma {TComma}
    PGo {TGo}
    PTake {TTake}
    PMark {TMark}
    PNothing {TNothing}
    PTurn {TTurn}
    PCase {TCase}
    POf {TOf}
    PEnd {TEnd}
    PLeft {TLeft}
    PRight {TRight}
    PFront {TFront}
    PSemicolon {TSemicolon}
    PEmpty {TEmpty}
    PLambda {TLambda}
    PDebris {TDebris}
    PAsteroid {TAsteroid}
    PBoundary {TBoundary}
    PUnderscore {TUnderscore}
    PIdent {TIdent $$}

%%

Program : Rules { Program $1 }

Rules : {- empty -}         {NoRule}
        | Rule Rules {SomeRules $1 $2}    

Rule : PIdent PArrow Commands PDot {Rule $1 $3}

Commands : {- empty -}               {NoCommand}
           | Command                 {SomeCommands $1 NoCommand}
           | Command PComma Commands {SomeCommands $1 $3}

Command : PGo                             {GoCommand}
          | PTake                         {TakeCommand}
          | PMark                         {MarkCommand}
          | PNothing                      {NothingCommand}
          | PTurn Direction               {TurnCommand $2}
          | PCase Direction POf Alts PEnd {CaseCommand $2 $4}
          | PIdent                        {RuleCommand $1}

Direction : PLeft    {LeftDir}
            | PRight {RightDir}
            | PFront {FrontDir}

Alts : {- empty -}           {NoAlt }
       | Alt                 {SomeAlts $1 NoAlt}
       | Alt PSemicolon Alts {SomeAlts $1 $3}

Alt : Pat PArrow Commands {Alt $1 $3}

Pat : PEmpty {EmptyPat}
      | PLambda {LambdaPat}
      | PDebris {DebrisPat}
      | PAsteroid {AsteroidPat}
      | PBoundary {BoundaryPat}
      | PUnderscore {UnderscorePat}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Exercise 2

data Program = Program Rules 
  deriving (Show)

data Rules = NoRule | SomeRules Rule Rules 
  deriving (Show) 

data Rule = Rule Identifier Commands
  deriving (Show)

data Commands = NoCommand | SomeCommands Command Commands 
  deriving (Show) 

data Command = GoCommand | TakeCommand | MarkCommand | NothingCommand | TurnCommand Direction | CaseCommand Direction Alts | RuleCommand Identifier
  deriving (Show) 

data Direction = LeftDir | RightDir | FrontDir
   deriving (Show)

data Alts = NoAlt | SomeAlts Alt Alts 
  deriving (Show) 

data Alt = Alt Pat Commands
  deriving (Show)

data Pat = EmptyPat | LambdaPat | DebrisPat | AsteroidPat | BoundaryPat | UnderscorePat
  deriving (Show)

type Identifier = String

}
