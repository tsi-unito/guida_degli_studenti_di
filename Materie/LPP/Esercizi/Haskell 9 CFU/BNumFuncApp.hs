module BNumFuncApp where

import Prelude hiding (succ, pred, div)
import PositiveNum( Mask( .. ), Positive( .. ), succ, add, pred, sub, mul)
import qualified PositiveNum as PN
import PositiveNumVsInteger ( int2Pos )
import qualified PositiveNumVsInteger as PNI
import PositiveNumOrder ( )
import GHC.Base (Applicative(..))

-- Notazione che include lo "zero" per un qualche insieme
-- di numeri positivi.
data BinNotation a where
  N0   :: BinNotation a
  Npos :: a -> BinNotation a
  deriving (Show)

instance Functor BinNotation where
  fmap :: (a -> b) -> BinNotation a -> BinNotation b
  fmap f N0 = N0
  fmap f (Npos x) = Npos (f x)
  -- (<$) :: a -> BinNotation b -> BinNotation a
  -- (<$) = _ 

-- Numeri naturali in notazione binaria a partire
-- dai numeri positivi.
type BN = BinNotation Positive

-- Funzioni unarie
succ :: BN -> BN
succ N0 = Npos XH
succ x  = fmap PN.succ x

pred :: BN -> BN
{-  La seguente definizione:
     
      pred = fmap PN.pred 
    
    è errata perché:
      
      pred (Npos XH)
      == fmap PN.pred (Npos XH)
      == Npos (PN.pred XH)
      == Npos XH

    Il valore (Npos XH) è un punto di "discontinuità"
    dovuto al fatto che in Positive non c'è valore 
    corrispondente allo zero.
-}
pred (Npos XH) = N0
pred x = fmap PN.pred x

instance Applicative BinNotation where
  pure :: a -> BinNotation a
  pure = Npos
  (<*>) :: BinNotation (a -> b) -> BinNotation a -> BinNotation b
  (Npos f) <*> (Npos x) = Npos (f x)
  _        <*> _        = N0
  liftA2 :: (a -> b -> c) -> BinNotation a -> BinNotation b -> BinNotation c
  liftA2 f (Npos x) (Npos y) = Npos (f x y)
  liftA2 _  _        _       = N0

-- Funzioni binarie
-- Due addizione alternative
add :: BN -> BN -> BN
add x y = fmap PN.add x <*> y

add' :: BN -> BN -> BN
add' = liftA2 PN.add

sub :: BN -> BN -> BN
sub  N0       _        = N0
sub  b        N0       = b
sub (Npos b) (Npos b') = case PN.sub b b' of
                         IsPos v -> Npos v
                         _       -> N0

mul:: BN -> BN -> BN
mul x = (<*>) (fmap PN.mul x)

instance Num BN where
  (+) :: BN -> BN -> BN
  (+) = BNumFuncApp.add
  (-) :: BN -> BN -> BN
  (-) = BNumFuncApp.sub
  (*) :: BN -> BN -> BN
  (*) = BNumFuncApp.mul
  negate :: BN -> BN
  negate x = x
  abs :: BN -> BN
  abs x = x
  signum :: BN -> BN
  signum N0 = N0
  signum _  = Npos XH
  fromInteger :: Integer -> BN
  fromInteger n | n <= 0    = N0
                | otherwise = Npos (PNI.int2Pos n)

-- necessario per test e divisione
instance Eq BN where
  (==) :: BN -> BN -> Bool
  (==)  N0       N0       = True
  (==) (Npos b) (Npos b') = (==) b b'
  (==)  _        _        = False

  (/=) :: BN -> BN -> Bool
  (/=) x y = not (x == y)

-- richiede PositiveNumOrder.hs
-- necessario per la definizione scelta del
-- quoziente 
instance Ord BN where
  (<) :: BN -> BN -> Bool
  (<)  N0       (Npos _ ) = True
  (<) (Npos p ) (Npos p') = (<) p p'
  (<)  _         _        = False

  (<=) :: BN -> BN -> Bool
  (<=) b  b' = b < b' || b == b'

  -- Per quanto riguarda 
  --
  -- compare :: BN -> BN -> Ordering
  --
  -- vedere ESERCIZIO n.ro 4 in Esercizi.hs
  --
  -- []

div :: BN -> BN -> BN
-- pre-condition d > 0
div N0 _ = N0
div n d
  | (<) n d   = N0 
  | otherwise =
    BNumFuncApp.succ (div (BNumFuncApp.sub n d) d)

-- Non è l'unico modo. Vedere BNumFuncAppQuoRem.hs