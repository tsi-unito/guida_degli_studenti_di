module PositiveNumOrder where

import Prelude hiding (succ, pred)
import PositiveNum ( Positive ( .. ) )


-- propedeutico alla definizione di (<):: BNum -> BNum -> Bool
data Comp where
  Lt :: Comp
  Eq :: Comp
  Gt :: Comp

comp :: Positive -> Positive -> Comp
comp  XH     XH     = Eq
comp  XH    (XO _)  = Lt
comp  XH    (XI _)  = Lt
comp (XO _)  XH     = Gt
comp (XI _)  XH     = Gt
comp (XO p) (XO p') = comp p p'
comp (XI p) (XI p') = comp p p'
comp (XO p) (XI p') = case comp p p' of
                      Lt -> Lt
                      Eq -> Lt
                      Gt -> Gt
comp (XI p) (XO p') = case comp p p' of
                      Lt -> Lt
                      Eq -> Gt
                      Gt -> Gt

instance Ord Positive where
  (<) :: Positive -> Positive -> Bool
  (<) p p' = case comp p p' of
             Lt -> True
             _  -> False

  (<=) :: Positive -> Positive -> Bool
  (<=) b b' = (<) b b' || (==) b b'
  
  -- Per quanto riguarda:
  --
  -- compare :: Positive -> Positive -> Ordering
  --
  -- vedere ESERCIZIO n.ro 4 in Esercizi.hs .
  --
  -- []
