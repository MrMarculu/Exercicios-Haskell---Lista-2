main :: IO ()
main = do
    putStr "Digite um texto: "
    val <- getLine
    print $ reverse val