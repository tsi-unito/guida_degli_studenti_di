module PositiveNum where
import Prelude hiding ( succ, pred )

-- numeri positivi in notazione binaria
data Positive where
  XH :: Positive 
  XO :: Positive -> Positive -- 2p
  XI :: Positive -> Positive -- 2p+1
  deriving (Show)
  
-- operazioni
succ :: Positive -> Positive
succ XH     = XO XH
succ (XO p) = XI p
succ (XI p) = XO (succ p)

pred :: Positive -> Positive
pred XH     = XH
pred (XO XH)  = XH
pred (XI p) = XO p 
pred (XO p) = XI (pred p)

add :: Positive -> Positive -> Positive
add XH p = succ p
add (XO p) XH = succ (XO p)
add (XI p) XH = succ (XI p)
add (XO m) (XO n) = XO (add m n)
add (XO m) (XI n) = XI (add m n)
add (XI m) (XI n) = XO (succ (add m n))
add (XI m) (XO n) = XI (add m n)

mul:: Positive -> Positive -> Positive
mul XH y = y
mul y XH = y
mul (XO m) (XO n) = XO (mul m n)
mul (XO m) (XI n) = XO (add (XI (mul m n)) m)
mul (XI m) (XO n) = XO (add (XI (mul m n)) n)
mul (XI m) (XI n) = succ (add (XI (XI (mul m n))) (XI (XI (add m n))))

-- propedeutico al testing
instance Eq Positive where
  (==) :: Positive -> Positive -> Bool  
  (==) XH XH = True
  (==) (XO p) (XO p') = (==) p p'
  (==) (XI p) (XI p') = (==) p p'
  (==) _ _ = False

  (/=) :: Positive -> Positive -> Bool  
  (/=) p = not . (==) p 

-- propedeutico alla definizione di sub
data Mask where
  IsNeg :: Mask
  IsNul :: Mask
  IsPos :: Positive -> Mask
  deriving (Show, 
            Eq) -- necessario per testing

sub :: Positive -> Positive -> Mask
sub XH     XH      = IsNul
sub XH     _       = IsNeg
sub m XH           = IsPos (pred m)
sub (XO m) (XO n) = case sub m n of
                    IsNeg -> IsNeg
                    IsNul -> IsNul
                    IsPos p -> IsPos (XO p)  
sub (XI m) (XO n) = case sub m n of
                    IsNeg -> IsNeg 
                    IsNul -> IsPos XH
                    IsPos p -> IsPos (succ (XO p)) 
sub (XO m) (XI n) = case sub m n of
                    IsNeg -> IsNeg
                    IsNul -> IsNeg
                    IsPos p -> IsPos (pred (XO p)) 
sub (XI m) (XI n) = case sub m n of
                    IsNeg -> IsNeg
                    IsNul -> IsNul
                    IsPos p -> IsPos (XO p) 
