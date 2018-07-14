{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage string =
    parseString (words string) where
        parseString (m:t:xs) =
            processMessageType m (read t :: Int) xs where
                processMessageType "I" timeStamp ys = LogMessage Info timeStamp (unwords ys)
                processMessageType "W" timeStamp ys = LogMessage Warning timeStamp (unwords ys)
                processMessageType "E" severity  (timeStampStr:ys) = let e         = Error severity
                                                                         timeStamp = read timeStampStr :: Int
                                                                         message   = unwords ys
                                                                     in LogMessage e timeStamp message
                processMessageType _   _         _                 = Unknown string
        parseString _        = Unknown string


parse :: String -> [LogMessage]
parse str = parseMessage <$> lines str


timestamp :: LogMessage -> TimeStamp
timestamp (LogMessage Info      timeStamp _) = timeStamp
timestamp (LogMessage Warning   timeStamp _) = timeStamp
timestamp (LogMessage (Error _) timeStamp _) = timeStamp
timestamp _                                  = error "No message with timestamp"


messageText :: LogMessage -> String
messageText (LogMessage Info      _ text) = text
messageText (LogMessage Warning   _ text) = text
messageText (LogMessage (Error _) _ text) = text
messageText (Unknown    text)             = text


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree                = tree
insert message     Leaf                = Node Leaf message Leaf
insert message     (Node left m right) | timestamp message < timestamp m  = Node (insert message left) m right
                                       | timestamp message > timestamp m  = Node left m (insert message right)
                                       | otherwise                        = error "same timestamp?"


build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = filterAndGetImportant slist
    where
        btree = build messages
        slist = inOrder btree

        filterAndGetImportant = foldr ((++) . getIfImportant) []

        getIfImportant (LogMessage (Error severity) _ text) | severity >= 50 = [text]
                                                            | otherwise      = []
        getIfImportant _                                    = []
