import System.IO
import Data.List
import Data.Char

data Tree = Node [Int] String Tree Tree | Leaf deriving Show

imprimir Leaf = return ()
imprimir (Node l p esq dir) = do imprimir esq
                                 putStrLn (p ++ " - " ++ show l)
                                 imprimir dir

aux n [] = []
aux n (x:xs) = (n, x): aux (n+1) xs

minusculo [] = []
minusculo (x:xs) = toLower x: minusculo xs

--separar o texto em linhas e a numeralas
numLines xs = aux 1 (lines xs)

--separar cada palavra e dizer sua linha
allNumWords' n [] = []
allNumWords' n (x:xs) = (n, x): allNumWords' n xs

allNumWords [] = []
allNumWords ((n,s):xs) = allNumWords' n (words s) ++ allNumWords xs

--Inserir elementos
ins e [] = [e]
ins e l@(x:xs)  |e == x = l
                |e < x = (e:l)
                |otherwise = (x:ins e xs)

--inserindo na arvore

insArv Leaf y x = Node [y] x  Leaf Leaf
insArv (Node i s esq dir) y x |x == s = Node (ins y i) s esq dir
                              |x < s = Node i s (insArv esq y x) dir
                              |otherwise = Node i s esq (insArv dir y x)

--Fazer o index
makeIndex [] arv = arv
makeIndex ((y,x):xs) arv = makeIndex xs (insArv arv y x)

main = do putStr "Arquivo: "
          hFlush stdout
          arq <- getLine
          txt <- readFile arq
          let minusculos = minusculo txt
          let linhas = numLines minusculos
          let palavras = allNumWords linhas
          let index = makeIndex palavras Leaf
          imprimir index


