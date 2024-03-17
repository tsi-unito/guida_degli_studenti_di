{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module EvalExceptionEsercizioConStatoVariabili where

import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int, int2BN )
import HelperTest ( terneFXY, errListFXY )

data Term where
    Tv :: Char -> Term 
    Tc :: BN -> Term
    Td :: Term -> Term -> Term
    deriving (Show)

type Exception = String
data E a where
    Rise   :: Exception -> E a
    Return :: a -> E a
    deriving (Show)

type State = [(Char, BN)]

look :: Char -> State -> BN
look n [] =  B.N0
look n ((k,v):s') = if n == k then v else look n s'

type S a = State -> (a ,State)

ev :: Term -> S (E BN)
...
    
-- TEST
s :: [(Char,BN)]
s = [('a', int2BN 1972), ('b', 2), ('c', int2BN 1)]
ok , nok :: Term
-- (1972/2)/23
ok  = Td (Td (Tv 'a') 
             (Tv 'b')) 
         (Tc (int2BN 23))
-- 1/0
nok = Td (Tv 'c') 
         (Tc (int2BN 0))

test :: IO ( )
test = do
    print "-------------- EvalException.ev"
    print (ev ok  s)
    print (ev nok s) -- "Div by 0"
