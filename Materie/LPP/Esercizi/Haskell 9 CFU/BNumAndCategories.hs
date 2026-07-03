module BNumAndCategories where

import Prelude hiding (succ, pred)
import HelperTest (errListFXY, errListFX, terneFXY, terneFX)
import PositiveNum (Positive ( .. ), succ, add, pred)
import qualified PositiveNum as P
import qualified PositiveNumTest as PT
import BNum ( succ, add, pred, BN (N0, Npos) )
import qualified BNum as B
import BNumVsInteger ( bN2Int, int2BN )
import qualified BNumVsInteger as BI

-- Confrontando EvalException.hs 
-- con EvalExceptionMonadic.hs
-- la differenza dovrebbe essere evidente:
-- la gestione della comunicazione del 
-- risultato tra i due processi che valutano
-- indipendentemente due sottoespressioni
-- è inglobata nella funzione 'bind' (>>=)
-- che, a tutti gli effetti può essere letta
-- come un'applicazione:
--  1.  prende un valore "ingabbiato" nella 
--      costruzione offerta da una Monade.
--      Significa che la "gabbia" può
--      inglobare informazioni utili per lo 
--      sviluppo dell'intero processo di 
--      calcolo, e 
--  2.  prende una funzione che dipende solo dal
--      valore "ingabbiato", quindi non da
--      informazione al contorno
--  3.  restituisce a sua volta un valore
--      "ingabbiandolo" opportunamente in accordo
--      con la struttura richiesta dalla Monade.

-- Interrogando ghci, si ottengono le seguenti
-- definizioni:
--
--   type Monad :: (* -> *) -> Constraint
--   class Applicative m => Monad m where
--     (>>=) :: m a -> (a -> m b) -> m b
--     (>>) :: m a -> m b -> m b
--     return :: a -> m a
--     {-# MINIMAL (>>=) #-}
--  
--   type Applicative :: (* -> *) -> Constraint
--   class Functor f => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b
--     GHC.Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c
--     (*>) :: f a -> f b -> f b
--     (<*) :: f a -> f b -> f a
--     {-# MINIMAL pure, ((<*>) | liftA2) #-}
--  
--   type Functor :: (* -> *) -> Constraint
--   class Functor f where
--     fmap :: (a -> b) -> f a -> f b
--     (<$) :: a -> f b -> f a
--     {-# MINIMAL fmap #-}
--
-- Quindi, BN deve poter essere istanza di 
-- un Functor, ma questo non è possibile perché
-- non è parametrico e non può costruire nuovi
-- oggetti o spazi di funzioni a partire da
-- oggetti e spazi di funzioni noti.
--
-- Interpretando la seguente linea:
--
--      instance Functor BN where
--
-- si ottiene l'errore:
--
--      • Expected kind ‘* -> *’, but ‘BN’ has kind ‘*’
--      • In the first argument of ‘Functor’, namely ‘BN’
--      In the instance declaration for ‘Functor BN
--
-- Occorre la struttura più flessibile in
-- BNumFuncApp.hs