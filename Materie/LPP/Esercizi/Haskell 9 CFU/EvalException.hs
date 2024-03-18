module EvalException where

import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int, int2BN )
import HelperTest ( terneFXY, errListFXY )
import Language ( Term ( .. ) )


-- Vedere ESERCIZIO n.ro 2 in Esercizi.hs
type Exception = String
data E a where
    Rise   :: Exception -> E a
    Return :: a -> E a
    deriving (Show)

ev :: Term -> E BN
ev (Tcon n) = Return n 
ev (Tdiv t1 t2) =
    let ev1 = ev t1 in
    let ev2 = ev t2 in 
    case (ev1, ev2) of 
    (Rise    s, _        ) -> Rise s
    (_        , Rise s   ) -> Rise s
    (Return v1, Return N0) -> Rise "Div by 0"
    (Return v1, Return v2) -> Return (B.div v1 v2)

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
    print "-------------- EvalException.ev"
    print (ev ok)
    print (ev nok) -- "Div by 0"
