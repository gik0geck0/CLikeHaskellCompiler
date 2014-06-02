module Main(main) where

import CLikeParser
import CLikeLexer
import CLikeVisitor

-- Lexing main
-- TODO: I want this to print out all the tokens that are found, but it doesn't quite work
-- main :: IO ()
-- main = do
--     getContents >>= printAlexToken . runAlexScan
-- 
-- printAlexToken :: Either String Token -> IO ()
-- printAlexToken (Left s) = print ("Error: " ++ s)
-- printAlexToken (Right t) = print t

-- Parsing main 
main = do
    cs <- getContents
    case parseExp cs of
        Left  e -> putStrLn ("error: " ++ e)
        Right e -> runPrinter e
