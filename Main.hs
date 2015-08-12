module Main where
  import Query
  import Data.List

  anyE :: (a -> Bool) -> [a] -> Bool
  anyE f [] = True
  anyE f xs = any f xs

  traverse :: String -> Query -> Bool
  traverse s (BooleanQuery xs)  = (all (traverse s) [q | Clause And q <- xs ]) && 
                                  (anyE (traverse s) [q | Clause Or q <- xs ]) &&
                                  not (any (traverse s) [q | Clause Not q <- xs ])
  traverse s (PhraseQuery ws)   = (unwords . reverse) ws `isInfixOf` s
  traverse s (TermQuery t)      = t `elem` words s

  --expl :: Query -> Bool
  --expl (BooleanQuery xs) = all (traverse s) [q | Clause And q <- xs ] && any (traverse s) [q | Clause Or q <- xs ] && not all (traverse s) [q | Clause Not q <- xs ]

  main = 
    --let query = (parse "(malpensa linate) +vado")
    print $ traverse "vado a malpensa e anche a linate" (parse "  (malpensa linate) +\"anche ah\"   ")

    --print $ expl $ parse "+Malpensa +Orio -Linate"

    -- Clause And Malpensa, Clause And Linate
    -- Clause Or Malpensa, Clause Not Linate



    -- [Clause] -> 


