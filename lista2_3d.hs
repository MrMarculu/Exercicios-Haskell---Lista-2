import System.Directory (doesFileExist)
import System.IO
import Control.Monad (unless, )

chkTipo :: String -> Maybe Int
chkTipo texto = case reads texto :: [(Int, String)] of
    [(n, "")] -> Just n
    _         -> Nothing

lerEntrada :: IO Int
lerEntrada = do
    entrada <- getLine
    case chkTipo entrada of
        Just n | n > 0 -> return n
               | otherwise -> putStrLn "Digite apenas números positivos, inteiros e não nulo" >> lerEntrada
        Nothing -> putStrLn "Digite apenas números inteiros!" >> lerEntrada

lerEadiciona :: Int -> [String] -> IO [String]
lerEadiciona 0 lista = return $ reverse lista
lerEadiciona numInter lista = do
    putStrLn $ "Digite a " ++ show numInter ++ "ª linha"
    entrada <- getLine
    lerEadiciona (numInter - 1) (entrada : lista)

verificarArquivo :: FilePath -> IO ()
verificarArquivo caminho = do
    verificado <- doesFileExist caminho
    unless verificado $ writeFile caminho ""

escreverArquivo :: FilePath -> [String] -> IO ()
escreverArquivo caminho lista = do
    --extrai lista
    case lista of
        [] -> return ()
    -- mapM_ é um map para mônada que descarta o resultado (m b)
        _  -> mapM_ (\linha -> appendFile caminho (linha ++ "\n")) lista
    putStr "Todas linhas adicionadas!"

main :: IO ()
main = do
    putStrLn "Quantas linhas gostaria de escrever? (positivos e não nulo)"
    qtdLinhas <- lerEntrada
    lista <- lerEadiciona qtdLinhas []
    verificarArquivo "./arquivo.txt"
    escreverArquivo "./arquivo.txt" lista