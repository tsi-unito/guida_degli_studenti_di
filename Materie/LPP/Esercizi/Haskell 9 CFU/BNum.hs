module BNum where

import Prelude hiding (succ, pred)
import PositiveNum ( Mask( .. ), Positive( .. ), succ, add, pred, sub, mul )
import qualified PositiveNum as P
import PositiveNumVsInteger ( int2Pos )

-- numeri naturali in notazione binaria
data BN where
    N0 :: BN -- 0
    Npos :: Positive -> BN
    deriving (Show)

-- funzioni unarie
succ :: BN -> BN
succ N0 = Npos XH
succ (Npos p) = Npos (P.succ p)

pred :: BN -> BN
pred N0 = N0
pred (Npos XH) = N0 -- mancava ...
pred (Npos p) = Npos (P.pred p)

-- funzioni binarie
add :: BN -> BN -> BN
add N0 y = y
add y  N0 = y
add (Npos p) (Npos p') = Npos (P.add p p')

sub :: BN -> BN -> BN
sub N0 y = N0
sub y  N0 = y
sub (Npos p) (Npos p') = 
  case P.sub p p' of
  IsPos v -> Npos v
  _       -> N0  

mul:: BN -> BN -> BN
mul N0 y = N0
mul y  N0 = N0
mul (Npos p) (Npos p') = Npos (P.mul p p')

-- altre funzioni
negate :: BN -> BN  
negate x = x  

abs :: BN -> BN
abs x = x

signum :: BN -> BN
signum N0 = N0
signum _  = Npos XH

fromInteger :: Integer -> BN
fromInteger n | n <= 0    = N0
              | otherwise = Npos (int2Pos n)

-- necessario per il testing
instance Eq BN where
  (==) :: BN -> BN -> Bool
  (==) N0 N0 = True
  (==) (Npos p) (Npos p') = (==) p p'
  (==) _ _ = False
  
  (/=) :: BN -> BN -> Bool
  (/=) x y = not (x == y)
