module HelperTest where

-- TEST FUNZIONI BINARIE
-- Terne argomento x, argomento y, valore f x y 
terneFXY :: (Integer -> Integer -> a) -> [(Integer, Integer, a)]
terneFXY f = [(x, y, f x y) | x <- [1..9], y <- [1..9]]
-- Filtra le terne in cui il risultato atteso non è quello ottenuto
errListFXY ::  Eq c => (a -> b -> c) -> [(a, b, c)] -> [(a, b, c)]
errListFXY f = filter (\(x, y, z) -> z /= f x y)

-- TEST FUNZIONI UNARIE
-- Terne argomento x, valore f x 
terneFX :: (Integer -> a) -> [(Integer, a)]
terneFX f = [(x, f x) | x <- [1..9]]
-- Filtra le terne in cui il risultato atteso non è quello ottenuto
errListFX ::  Eq b => (a -> b) -> [(a, b)] -> [(a, b)]
errListFX f = filter (\(x, z) -> z /= f x )
