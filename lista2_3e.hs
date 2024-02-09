import System.IO
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Control.Monad (unless,sequence)
import Data.Maybe (catMaybes)

lerLinha :: [String] -> [Maybe Double]
lerLinha = map readMaybe

verificarArquivo :: FilePath -> IO Bool
verificarArquivo caminho = do
    doesFileExist caminho

lerCaminho :: IO FilePath
lerCaminho = do
    putStrLn "Digite o caminho para o arquivo: "
    caminho <- getLine
    existe <- verificarArquivo caminho
    -- não é possível usar unless pois ele precisa retorna um IO () e uso de case é redudante
    if not existe then do
        putStrLn "caminho especificado nao existe!"
        lerCaminho
    else
        return caminho

lerArquivo :: IO [[Maybe Double]]
lerArquivo = do
    caminho <- lerCaminho
    linhas <- lines <$> readFile caminho
    let listaNum = map (lerLinha . words) linhas
    return listaNum

--pega maior valor de cada linha e retorna uma lista com todos os valores
maioresVal :: [[Maybe Double]] -> [Maybe Double]
maioresVal = map maiorValorLinha
  where
    maiorValorLinha :: [Maybe Double] -> Maybe Double
    maiorValorLinha linha
      |Nothing `elem` linha = Nothing
      |otherwise = Just . maximum $ catMaybes linha

-- deveria colocar um retorno bool, mas tá tarde e eu to com fome, dps arrumo
geraArq :: [[Maybe Double]] -> IO ()
geraArq listaNum = do
    let valores = maioresVal listaNum
        (valoresValidos, erros) = foldl processaLinha ([], []) (zip [1..] valores)
    -- lembrete: adicionar checagem caso arquivo já exista.
    writeFile "resultado.txt" (unlines $ map show valoresValidos ++ ["Erros nas linhas: "] ++ map show erros)

    where
      processaLinha (vs, es) (n, Just v) = (v:vs, es)
      processaLinha (vs, es) (n, Nothing) = (vs, n:es)

main :: IO ()
main = do
    listaNum <- lerArquivo
    geraArq listaNum
    putStrLn "adicionado lista de resultados!"
    

-- a = [[Just 1.0,Just 2.0],[Just 2.0,Just 4.0],[Just 9.0,Just 7.0],[Just 455.0,Just 300.0]]
-- b = [[Just 1.0,Just 2.0],[Just 2.0,Nothing],[Just 9.0,Just 7.0],[Just 455.0,Just 300.0]]