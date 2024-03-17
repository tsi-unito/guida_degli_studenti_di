module BNumFuncAppTest where

import HelperTest ( errListFXY, terneFX, terneFXY, errListFX )

import BNumFuncAppVsInteger ( int2BN )
import BNumFuncApp ( div )
import qualified BNumFuncApp as B

-- TEST.
test :: IO ( )
test = do
    print "===== BNumFuncApp Test Inizio"
    print "-------------- BNumFuncApp.(<): lista vuota <==> no errori"
    let testListLt = terneFXY (\x y -> (<) (int2BN x) (int2BN y))
    let errorFilterLt = (<)
    print (errListFXY errorFilterLt testListLt)

    print "-------------- BNumFuncApp.div: lista vuota <==> no errori"
    let testListDiv = terneFXY (\x y -> B.div (int2BN x) (int2BN y))
    let errorFilterDiv x y = int2BN (Prelude.div x y)
    print (errListFXY errorFilterDiv testListDiv)

    print "===== BNumFuncApp Test Fine"
