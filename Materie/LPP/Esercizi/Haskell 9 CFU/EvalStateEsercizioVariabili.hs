module EvalStateEsercizioVariabili where
import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int, int2BN )
import HelperTest ( terneFXY, errListFXY )



-- Linguaggio di termini Term aggiornato per poter
-- usare variabili identificate da un nome, che è 
-- l'argomento del costruttore Tv.
-- 
-- 1ma IPOTESI. Un identificatore è un singolo 
-- carattere.
data Term' where
    Tv :: Char -> Term' 
    Tc :: BN -> Term' 
    Td :: Term' -> Term' -> Term' 
    deriving (Show)

-- 2d IPOTESI. Uno stato è un "dizionario":
-- lista di coppie ('carattere', b)::((,) Char BN)
type State' = 
    [((,) Char BN)]

-- Dati un nome di variabile e uno stato "dizionario"
-- è utile definire una funzione che restituisce il 
-- valore della variabile data. 
-- 
-- Cominciare con una definizione di funzione che,
-- anche in caso di insuccesso, restituisce un valore
-- di tipo BN.
look :: Char -> State' -> BN
look = 
    where
        look' :: Char -> State' -> BN
        look' x [] = int2BN 0
        look' x ((y, b):s) = if x == y then b else look' x s
        
-- Il costruttore di tipo del risultato fornito
-- dall'interprete dei termini in Term' permette di
-- calcolare il valore di termini in cui compaiono
-- variabili il cui valore dipende dallo stato di
-- valutazione.
type S' a = (->) State' ((,) a State')

-- Segue l'interprete da Term' a S' BN in grado di 
-- usare lo stato per stabilire il valore da associare 
-- alle variabili di un termine da valutare.
evEx :: Term' -> S' BN
evEx = 
    where
        ev' :: Term' -> State' -> (BN, State')
        ev' (Tc n)     s = (n, s)
        ev' (Tv x)     s = (look x s, s)
        ev' (Td t1 t2) s = let (n1, s1) = ev' t1 s
                               (n2, s2) = ev' t2 s
                           in (B.div n1 n2, s2)

-- ESTENSIONI
-- 
-- 1.   Gestire l'insuccesso di look con un tipo adeguato.
-- 2.   Estendere il linguaggio con un costruttore Ta da 
--      interpretare come assegnazione che altera lo stato,
--      o modificando il valore associato a una variabile o
--      estendendo lo stato
--      