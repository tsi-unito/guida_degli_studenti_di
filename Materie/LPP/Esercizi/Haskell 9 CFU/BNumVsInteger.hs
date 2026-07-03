module BNumVsInteger where

import PositiveNumVsInteger ( pos2Int, int2Pos )
import qualified PositiveNumVsInteger as P
import BNum ( BN (Npos, N0), fromInteger )

-- Conversioni.
bN2Int :: BN -> Integer
bN2Int  N0      = 0
bN2Int (Npos p) = P.pos2Int p

int2BN :: Integer -> BN
int2BN = BNum.fromInteger

