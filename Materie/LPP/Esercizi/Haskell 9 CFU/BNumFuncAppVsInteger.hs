module BNumFuncAppVsInteger where

import PositiveNumVsInteger ( pos2Int )
import qualified PositiveNumVsInteger as P
import BNumFuncApp ( BinNotation ( N0, Npos ), BN )

-- Conversioni.
bN2Int :: BN -> Integer
bN2Int  N0      = 0
bN2Int (Npos p) = P.pos2Int p

int2BN :: Integer -> BN
int2BN  = fromInteger
