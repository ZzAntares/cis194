{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
-- "E 2 562 help help" -> LogMessage Error 2 562 "help help"
parseMessage :: String -> LogMessage
parseMessage message =
  case words message of
    ("I" : ts : xs) -> LogMessage Info (read ts :: Int) (unwords xs)
    ("W" : ts : xs) -> LogMessage Warning (read ts :: Int) (unwords xs)
    ("E" : code : ts : xs) ->
      LogMessage (Error (read code :: Int)) (read ts :: Int) (unwords xs)
    msg -> Unknown $ unwords msg

-- Multi-line string of errors to [LogMessage]
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert message@LogMessage{} Leaf = Node Leaf message Leaf
insert message@(LogMessage _ ts _) (Node left omessage@(LogMessage _ ots _) right)
  | ts > ots = Node left omessage (insert message right)
  | otherwise = Node (insert message left) omessage right
insert _ tree = tree

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map toMessage . inOrder . build . filter isSevereError
  where
    isSevereError (LogMessage (Error severity) _ _) = severity >= 50
    isSevereError _ = False
    toMessage (LogMessage _ _ message) = message
    toMessage (Unknown _) = ""  -- Unreachable since Unknowns are filtered out
