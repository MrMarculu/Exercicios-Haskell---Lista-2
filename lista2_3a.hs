ePar :: Int -> String
ePar x 
    | even x = "Par"
    | otherwise = "Impar"

chkTipo :: String -> Maybe Int
-- tenta converter a entrada para um suposto int
chkTipo input = case reads input :: [(Int,String)] of
    [(val,"")] -> Just val
    _          -> Nothing 

lerEntrada :: IO Int
lerEntrada = do
    -- Para fazer o parse, é necessário ser string
    entrada <- getLine
    case chkTipo entrada of
        Just val -> return val
        Nothing -> do
            putStr "Digite apenas valores inteiros! : "
            lerEntrada

main :: IO()
main = do
    putStr "Digite um número inteiros: "
    val <- lerEntrada
    print (ePar val)