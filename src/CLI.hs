module CLI where

cli :: IO ()
cli = do
    line <- getLine
    putStrLn ("got this: " <> line)