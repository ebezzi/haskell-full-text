module Main where
  import Query
  import Data.List

  traverse :: String -> Query -> Bool
  traverse s (BooleanQuery xs)  = all (traverse s) xs
  traverse s (PhraseQuery ws)   = (unwords . reverse) ws `isInfixOf` s
  traverse s (TermQuery t)      = t `elem` words s

  main = 
    print $ traverse "ciao a tutti da Emanuele Bezzi" (parse "\"Emanuele Bezzi\"")