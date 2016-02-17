{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, NoMonomorphismRestriction #-}

import Database.MongoDB
import Control.Monad.Trans (liftIO)

main = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "social" run
    close pipe

-- >>= :: (Monad m) => ma -> (a -> mb) -> mb
-- Monad: Action IO
-- a: [Document]

run :: Action IO ()
run = do
    allTeams >>= printDocs "All Teams"

allTeams :: Action IO [Document]
allTeams = do
  cursor <- find (select [] "stream")
  enum cursor

enum :: Cursor -> Action IO [Document]
enum cursor = nextBatch cursor >>= \res -> case res of
  [] -> return []
  xs -> do
    future <- enum cursor
    return (xs ++ future)

printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . include ["_id"]) docs




enum :: Cursor -> 