{
module Parser where
import Scanner
}

%name parseCalc
%tokentype { Token }
%error { parseError }



%token
    PArrow { Arrow }
    PDot  { Dot }
    PComma {Comma}
    PGo {Go}
    PTake {Take}
    PMark {Mark}
    PNothing {Nothingg}
    PTurn {Turn}
    PCase {Case}
    POf {Of}
    PEnd {End}
    PLeft {Leftt}
    PRight {Rightt}
    PFront {Front}
    PSemicolon {Semicolon}
    PEmpty {Empty}
    PLambda {Lambda}
    PDebris {Debris}
    PAsteroid {Asteroid}
    PBoundary {Boundary}
    PUnderscore {Underscore}
    PIdent {Ident $$}

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
          | PMark                         {MarkComand}
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

data Identifier = Identifier String
   deriving (Show)

}
