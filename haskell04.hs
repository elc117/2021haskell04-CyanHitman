-- Prática 04 de Haskell
-- Nome: Gabriel da Silva França

import Text.Printf

--1
faixaIdoso :: Int -> String
faixaIdoso idade
    | idade > 59 && idade < 65 = "IDO64"
    | idade > 64 && idade < 70 = "IDO69"
    | idade > 69 && idade < 75 = "IDO74"
    | idade > 74 && idade < 80 = "IDO79"
    | idade > 80  = "IDO80"
    | otherwise = "ND" 

--2
classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos saida = [(nome, idade, faixaIdoso idade) | (nome, idade) <- saida]

--3
classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' tupla = map (\(nome, idade) -> (nome, idade, faixaIdoso idade)) tupla

--4
strColor :: (Int,Int,Int) -> String
strColor (r,g,b) = printf "rgb (%d, %d, %d)" r g b

--5
genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (x, y) r = take n [(x,y,r) | x <- [fst(x,y), (fst(x,y) + 2 * r)..]]


--6
genReds :: Int -> [(Int,Int,Int)] 
genReds n = [(80+i*10, 0, 0) | i <- [0..n-1]]




