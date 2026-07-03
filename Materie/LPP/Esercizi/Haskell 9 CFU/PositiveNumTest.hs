module PositiveNumTest where

import HelperTest (terneFX, errListFX, terneFXY, errListFXY)

import PositiveNumVsInteger ( int2Pos )
import PositiveNum ( Positive( .. ), 
                     Mask( .. ), 
                     succ, 
                     add, 
                     pred, 
                     sub,
                     mul
                     )
import qualified PositiveNum as PN

-- -------------------------
test :: IO ()
test = do
    print "===== Positive Test Inizio"

    print "-------------- Positive.pred: lista vuota <==> no errori"
    let testListPred = terneFX (PN.pred . int2Pos)
    let errorFilterPred x = int2Pos (x - 1)
    print (errListFX errorFilterPred testListPred)

    print "-------------- Positive.succ: lista vuota <==> no errori"
    let testListSucc = terneFX (PN.succ . int2Pos)
    let errorFilterSucc x = int2Pos (x + 1)
    print (errListFX errorFilterSucc testListSucc)

    print "-------------- Positive.sub: lista vuota <==> no errori"
    let testListSub = terneFXY (\x y -> PN.sub (int2Pos x) (int2Pos y))
    let errorFilterSub x y
          | x - y >  0 = IsPos (int2Pos (x - y))
          | x - y == 0 = IsNul
          | otherwise  = IsNeg
    print (errListFXY errorFilterSub testListSub)

    print "-------------- Positive.add: lista vuota <==> no errori"
    let testListAdd = terneFXY (\x y -> PN.add (int2Pos x) (int2Pos y))
    let errorFilterAdd x y = int2Pos (x + y)
    print (errListFXY errorFilterAdd testListAdd)

    print "-------------- Positive.mul: lista vuota <==> no errori"
    let testListMul = terneFXY (\x y -> PN.mul (int2Pos x) (int2Pos y))
    let errorFilterMul x y = int2Pos (x * y)
    print (errListFXY errorFilterMul testListMul)

    print "===== Positive Test Fine"
