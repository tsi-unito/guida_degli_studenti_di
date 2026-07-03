module EvalBasic where

import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int, int2BN )
import HelperTest ( terneFXY, errListFXY )
import Language ( Term ( .. ) )











ev :: Term -> BN
ev (Tcon n)   = n -- unit n == id n
ev (Tdiv d n) = 
    let evd = ev d
        evn = ev n
    in B.div evd evn


-- TEST
ok , nok :: Term
-- (1972/2)/23
ok  = Tdiv (Tdiv (Tcon (int2BN 1972)) 
                 (Tcon (int2BN 2))) 
           (Tcon (int2BN 23))
-- 1/0
nok = Tdiv (Tcon (int2BN 1)) 
           (Tcon (int2BN 0))

test :: IO ( )
test = do
    print "-------------- EvalBasic.ev"
    print (bN2Int (ev ok))
    print (bN2Int (ev nok)) -- stack overflow