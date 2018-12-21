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

Rules : {- empty -}         {[]}
        | Rule Rules {$1 : $2}    

Rule : PIdent PArrow Commands PDot {Rule $1 $3}

Commands : {- empty -}               {[]}
           | Command                 {$1 : []}
           | Command PComma Commands {$1 : $3}

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

Alts : {- empty -}           {[]}
       | Alt                 {$1 : []}
       | Alt PSemicolon Alts {$1 : $3}

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

data Program = Program [Rule] 
  deriving (Show)

data Rule = Rule Identifier [Command]
  deriving (Show)

data Command = GoCommand | TakeCommand | MarkCommand | NothingCommand | TurnCommand Direction | CaseCommand Direction [Alt] | RuleCommand Identifier
  deriving (Show) 

data Direction = LeftDir | RightDir | FrontDir
   deriving (Show)

data Alt = Alt Pat [Command]
  deriving (Show)

data Pat = EmptyPat | LambdaPat | DebrisPat | AsteroidPat | BoundaryPat | UnderscorePat
  deriving (Show)

type Identifier = String

}
