module PositiveNumVsInteger where

import Prelude hiding ( succ )
import PositiveNum ( Positive( .. ), succ )

-- Conversioni Positive
pos2Int :: Positive -> Integer
pos2Int  XH    = 1
pos2Int (XO p) = 2 * pos2Int p
pos2Int (XI p) = 1 + 2 * pos2Int p

-- Prima versione
int2Pos' ::  Integer -> Positive
int2Pos' x | x <= 1 = XH
           | otherwise = case mod x 2 of
                         0 -> XO (int2Pos' (Prelude.div x 2))
                         1 -> XI (int2Pos' (Prelude.div x 2))

-- Versione migliorata
int2Pos :: Integer -> Positive
int2Pos x | x <= 1 = XH
          | otherwise = succ (int2Pos (x - 1))