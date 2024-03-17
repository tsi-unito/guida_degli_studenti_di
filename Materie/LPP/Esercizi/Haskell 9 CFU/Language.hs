module Language where

import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int )
import HelperTest ( terneFXY, errListFXY )

data Term where
    Tcon :: BN   -> Term
    Tdiv :: Term -> Term -> Term
    deriving (Show)

-- Interprete ovvio basato sulla funzione
-- built-in Prelude.div
term2Int :: Term -> Integer
term2Int (Tcon b)    = bN2Int b
term2Int (Tdiv t t') = 
    Prelude.div (term2Int t )
                (term2Int t')
                
-- Siamo però interessati a interpretare:
-- - CalcLanguage.Tcon come numeri binari
-- - CalcLanguage.Tdiv come divisione intera
--   tra numeri binari.


