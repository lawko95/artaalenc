{
module Main (main) where

}

%wrapper "basic"
$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				    ;
  "--".*				    ;
  \->                       {\s->Arrow}
  \.                        {\s->Dot}
  \,                        {\s->Comma}
  go                        {\s->Go}
  take                      {\s->Take}
  mark                      {\s->Mark}
  nothing                   {\s->Nothingg}
  turn                      {\s->Turn}
  case                      {\s->Case}
  of                        {\s->Of}
  end                       {\s->End}
  left                      {\s->Leftt}
  right                     {\s->Rightt}
  front                     {\s->Front}
  \;                        {\s->Semicolon}
  empty                     {\s->Empty}
  lambda                    {\s->Lambda}
  debris                    {\s->Debris}
  asteroid                  {\s->Asteroid}
  boundary                  {\s->Boundary}
  \_                        {\s->Underscore}
  [$alpha $digit \+ \-]+    {\s->Ident s}
{
-- Each action has type :: String -> Token

-- The token type:
data Token =    Arrow | Dot | Comma | Go | Take | Mark | Nothingg | Turn | Case | Of | End | Leftt | Rightt | Front | Semicolon 
                | Empty | Lambda | Debris | Asteroid | Boundary | Underscore | Ident String
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}