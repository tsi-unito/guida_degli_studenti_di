{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module BNumFuncAppQuotient where

import Prelude hiding (succ, pred, div)
import PositiveNum( Mask( .. ), Positive( .. ), succ, add, pred, sub, mul)
import qualified PositiveNum as PN
import PositiveNumVsInteger ( int2Pos )
import qualified PositiveNumVsInteger as PNI
import PositiveNumOrder ( )
import qualified PositiveNumOrder as PNO
import BNumFuncApp ( BN )
import qualified BNumFuncApp as BN


quoRem :: BN.BN -> BN.BN -> (BN.BN, BN.BN)
-- "Diviso BN.N0" non dà errore e termina.
-- Da gestire opportunamente
quoRem (BN.Npos (XO m)) BN.N0 = 
  let (q', r') = quoRem (BN.Npos m) BN.N0 
  in (q', BN.N0)
-- "Diviso 0" non dà errore e termina.
-- Da gestire opportunamente
quoRem (BN.Npos (XI m)) BN.N0 = 
  let (q', r') = quoRem (BN.Npos m) BN.N0 
  in (q', BN.N0)
-- Casi con divisore maggiore di BN.N0
quoRem BN.N0 _ = (BN.N0 , BN.N0)
quoRem (BN.Npos XH) n = (BN.N0, BN.Npos XH)
quoRem m (BN.Npos XH) = (m, BN.N0)
--
-- quoRem (XO m) d =
--   let (q, r) = quoRem m d
--   in if (2 * r) < d 
--         then (2 * q, 2 * r) 
--         else (2 * q + 1, (2 * r) - d)
quoRem (BN.Npos (XO m)) d =
  let (q, r) = quoRem (BN.Npos m) d
  in if ((BN.Npos (XO XH)) * r) < d 
     then ((BN.Npos (XO XH)) * q
          ,(BN.Npos (XO XH)) * r) 
     else ((BN.Npos (XO XH)) * q + (BN.Npos XH)
          ,((BN.Npos (XO XH)) * r) - d)
--
-- quoRem (XI m) d =
--   let (q, r) = quoRem m d
--   in if (2 * r + 1) < d 
--      then (2 * q, 2 * r + 1) 
--      else (2 * q + 1, (2 * r + 1) - d)
quoRem (BN.Npos (XI m)) d =
  let (q, r) = quoRem (BN.Npos m) d
  in if ((BN.Npos (XO XH)) * r) < d 
     then ((BN.Npos (XO XH)) * q
          ,(BN.Npos (XO XH)) * r + (BN.Npos XH)) 
     else ((BN.Npos (XO XH)) * q + (BN.Npos XH)
          ,((BN.Npos (XO XH)) * r + (BN.Npos XH)) - d)
-- Fare qualche test?