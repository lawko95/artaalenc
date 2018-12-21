{
module Scanner where
}

%wrapper "basic"
$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				    ;
  "--".*				    ;
  \->                       {\s->TArrow}
  \.                        {\s->TDot}
  \,                        {\s->TComma}
  go                        {\s->TGo}
  take                      {\s->TTake}
  mark                      {\s->TMark}
  nothing                   {\s->TNothing}
  turn                      {\s->TTurn}
  case                      {\s->TCase}
  of                        {\s->TOf}
  end                       {\s->TEnd}
  left                      {\s->TLeft}
  right                     {\s->TRight}
  front                     {\s->TFront}
  \;                        {\s->TSemicolon}
  Empty                     {\s->TEmpty}
  Lambda                    {\s->TLambda}
  Debris                    {\s->TDebris}
  Asteroid                  {\s->TAsteroid}
  Boundary                  {\s->TBoundary}
  \_                        {\s->TUnderscore}
  [$alpha $digit \+ \-]+    {\s->TIdent s}
{
-- Each action has type :: String -> Token

-- The token type:
data Token =    TArrow | TDot | TComma | TGo | TTake | TMark | TNothing | TTurn | TCase | TOf | TEnd | TLeft | TRight | TFront | TSemicolon 
                | TEmpty | TLambda | TDebris | TAsteroid | TBoundary | TUnderscore | TIdent String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}