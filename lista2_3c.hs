delta :: (Floating a) => (a, a, a)-> a
delta (a,b,c) = deltaVal
    where
        deltaVal = (b*b) - (4*(a*c))

fQua :: (Floating a,Ord a) => (a,a,a) -> Maybe (a,a)
fQua (a,b,c)
    | a < 0        = Nothing
    | deltaVal < 0 = Nothing
    | otherwise    = Just (fQuaValPos,fQuaValNeg)
        where
        deltaVal = delta (a,b,c)
        fQuaValPos = ((-b) + sqrt deltaVal) / 2*a
        fQuaValNeg = ((-b) - sqrt deltaVal) / 2*a


chkTipo :: String -> Maybe Float
chkTipo entrada = case reads entrada :: [(Float,String)] of
    [(n,"")] -> Just n
    _        -> Nothing

lerEntrada :: IO Float
lerEntrada = do
    entrada <- getLine
    case chkTipo entrada of
        Just val -> return val
        Nothing  -> do
            putStrLn "Digite um valor numerico! : "
            lerEntrada

main :: IO ()
main = do
    putStrLn "\tCalculadora de funcao quadratica!\nDigite o valor de A: "
    a <- lerEntrada
    putStrLn "Digite o valor de B:"
    b <- lerEntrada
    putStrLn "Digite o valor de C"
    c <- lerEntrada
    case fQua (a,b,c) of
        Just (x1,x2) -> case (x1,x2) of
                            _ | x1 == x2 -> do
                                        putStrLn "O valor de X é unico! Ele é: "
                                        putStrLn ("X = " ++ show x1)
                              | otherwise -> do
                                        putStrLn "O valor dos Xs é:"
                                        putStrLn ("X1 = " ++ show x1 ++ ", X2 = " ++ show x2)
        Nothing -> do
            putStrLn "A funcao nao possui valor real!" 