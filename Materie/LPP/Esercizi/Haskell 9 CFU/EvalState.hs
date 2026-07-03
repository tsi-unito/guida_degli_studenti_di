module EvalState where

import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int, int2BN )
import HelperTest ( terneFXY, errListFXY )
import Language ( Term ( .. ) )

type State = Int
-- L'operatore '->' è il costruttore
type S a = State -> (a, State)


ev :: Term -> S BN
ev (Tcon n)     = \s -> (n, s + 1)
ev (Tdiv t1 t2) = \s -> let (n1, s1) = ev t1 s
                            (n2, s2) = ev t2 s
                        in (B.div n1 n2, s2 + 1)
ev (Tsum t1 t2) = \s -> let (n1, s1) = ev t1 s
                            (n2, s2) = ev t2 s
                        in (B.sum n1 n2, s2 + 1)

-- TEST
ok , nok :: Term
ok  = Tdiv (Tdiv (Tcon (int2BN 1972)) 
                 (Tcon (int2BN 2   ))) 
           (Tcon (int2BN 23))
nok = Tdiv (Tcon (int2BN 1)) (Tcon (int2BN 0))

test :: IO ( )
test = do

   print "-------------- EvalState.ev"
   print (ev ok 0) -- con 0 offset del conteggio
   print (ev ok 3) -- con 3 offset del conteggio
   -- print (ev nok 0) -- stack overflow