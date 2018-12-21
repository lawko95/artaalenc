{
module Parser where
import Scanner as L
import Arrow as A 
}

%name parseCalc
%tokentype { Token }
%error { parseError }



%token
    PArrow { L.Arrow }
    PDot  { L.Dot }
    PComma {L.Comma}
    PGo {L.Go}
    PTake {L.Take}
    PMark {L.Mark}
    PNothing {L.Nothingg}
    PTurn {L.Turn}
    PCase {L.Case}
    POf {L.Of}
    PEnd {L.End}
    PLeft {L.Leftt}
    PRight {L.Rightt}
    PFront {L.Front}
    PSemicolon {L.Semicolon}
    PEmpty {L.Empty}
    PLambda {L.Lambda}
    PDebris {L.Debris}
    PAsteroid {L.Asteroid}
    PBoundary {L.Boundary}
    PUnderscore {L.Underscore}
    PIdent {L.Ident $$}

%%

Program : Rules { Program $1 }

Rules :  {- empty -}            {NoRule }
        | Rule                  {SomeRules $1 NoRule}
        | Rule PComma Rules   {SomeRules $1 $3}    

Rule : PIdent PArrow Commands PDot {Rule $1 $3}

Commands : {- empty -}          {NoCommand }
           | Command Commands {SomeCommands $1 $2}

Command : PGo {GoCommand}
          | PTake {TakeCommand}
          | PMark {MarkComand}
          | PNothing {NothingCommand}
          | PTurn Direction {TurnCommand $2}
          | PCase Direction POf Alts PEnd {CaseCommand $2 $4}
          | PIdent {RuleCommand $1}

Direction : PLeft {LeftDir}
            | PRight {RightDir}
            | PFront {FrontDir}

Alts : {- empty -}           {NoAlt }
       | Alt                 {SomeAlt $1 Norule}
       | Alt PSemicolon Alts {SomeAlt $1 $3}

Alt : Pat PArrow Commands {Alt $1 $3}

Pat : PEmpty {EmptyPat}
      | PLambda {LambdaPat}
      | PDebris {DebrisPat}
      | PAsteroid {AsteroidPat}
      | PBoundary {BoundaryPat}
      | PUnderscore {UnderscorePat}

Identifier : PIdent {Identifier $1}


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
  deriving (Show) Not using this anymore

data Command = GoCommand | TakeCommand | MarkCommand | NothingCommand | TurnCommand Direction | CaseCommand Direction Alts | RuleCommand Identifier
  deriving (Show) 

data Direction = LeftDir | RightDir | FrontDir
   deriving (Show)

data Alts = NoAlt | SomeAlt Alt Alts 
  deriving (Show) 

data Alt = Alt Pat Commands
  deriving (Show)

data Pat = EmptyPat | LambdaPat | DebrisPat | AsteroidPat | BoundaryPat | UnderscorePat
  deriving (Show)

data Identifier = Identifier String
   deriving (Show)

}
