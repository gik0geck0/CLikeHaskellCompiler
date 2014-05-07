module Main(main) where

import CLikeParser

-- main = getContents >>= print . calc . lexer
main = do
    cs <- getContents
    case parseExp cs of
        Left  e -> putStrLn ("error: " ++ e)
        Right e -> print e


