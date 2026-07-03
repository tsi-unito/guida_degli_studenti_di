module BNumTest where

import HelperTest ( errListFX, errListFXY, terneFX, terneFXY )
import BNumVsInteger ( int2BN )

import BNum ( BN ( N0 ), succ, add, pred, sub, mul )
import qualified BNum as B

---------------------------
test :: IO ()
test = do
    print "===== BNum Test Inizio"

    print "--------------BNum.pred: lista vuota <==> no errori"
    let testListPred = terneFX (B.pred . int2BN)
    let errorFilterPred x = int2BN (x - 1)
    print (errListFX errorFilterPred testListPred)

    print "--------------BNum.succ: lista vuota <==> no errori"
    let testListSucc = terneFX (B.succ . int2BN)
    let errorFilterSucc x = int2BN (x + 1)
    print (errListFX errorFilterSucc testListSucc)

    print "--------------BNum.sub: lista vuota <==> no errori"
    let testListSub = terneFXY (\x y -> B.sub (int2BN x) (int2BN y))
    let errorFilterSub x y
          | x - y >  0 = int2BN (x - y)
          | otherwise = N0
    print (errListFXY errorFilterSub testListSub)

    print "--------------BNum.add: lista vuota <==> no errori"
    let testListAdd = terneFXY (\x y -> B.add (int2BN x) (int2BN y))
    let errorFilterAdd x y = int2BN (x + y)
    print (errListFXY errorFilterAdd testListAdd)

    print "--------------BNum.mul: lista vuota <==> no errori"
    let testListMul = terneFXY (\x y -> B.mul (int2BN x) (int2BN y))
    let errorFilterMul x y = int2BN (x * y)
    print (errListFXY errorFilterMul testListMul)

    print "===== BNum Test Fine"
