From LPP Require Import PositiveNum.
Module PN := PositiveNum.
From LPP Require Import PositiveNumOrder.
Module PNO := PositiveNumOrder.
From LPP Require Import PositiveNumVsInteger.
Module PNI := PositiveNumVsInteger.

Notation "f ; g" :=
  (fun x =>  g (f x))
  (at level 60, right associativity) .
  (* : scope. *)

(** * [BinNotation a] is an Applicative Functor *)
Inductive BinNotation (a: Set) : Set :=
|  N0   : BinNotation a
|  Npos : a -> BinNotation a .

Definition fmap {a b: Set} (f: a -> b) (x: BinNotation a) : BinNotation b :=
match x with 
| N0   _   => N0 _
| Npos _ x => Npos _ (f x)
end.

Definition pure {a: Set} (x: a) : BinNotation a := Npos _ x .

(* starApp <==> <*> *)
Definition starApp {a b: Set} (f: BinNotation (a -> b)) (x: BinNotation a) : BinNotation b :=
match f, x with
| Npos _ f, Npos _ x => Npos _ (f x)
| _       , _        => N0 _
end .

Notation "x <*> y" :=
  (starApp x y)
  (at level 61, left associativity) .
  (* : scope. *)

Definition liftA2 {a b c: Set} (f: a -> b -> c) (x: BinNotation a) (y: BinNotation b) : BinNotation c :=
match x, y with
| Npos _ x, Npos _ y => Npos _ (f x y)
|  _      ,   _      => N0 _
end .

(** * Binary Numbers [BN] as instance of [BinNotation] *)
Definition BN := BinNotation PN.Positive .

(** *** Operations to let [BN] a numerical system *)
Definition succ (x: BN) : BN :=
match x with
| N0 _ => Npos _ PN.XH
| x    => fmap PN.succ x
end .
    
Definition pred (x: BN) : BN :=
match x with
| N0 _ => N0 _
| Npos _ XH => N0 _
end .

Proposition BinNotation_Positive_not_Functor:
  exists (b: BN), b <> (pred ; succ) b.  
Proof. intros. exists (N0 _). 
unfold succ. unfold pred. 
intro F. discriminate F. 
Qed.

Definition add (x: BN) (y: BN) : BN := starApp (fmap PN.add x) y .

Definition sub (b b': BN) : BN :=
match b, b' with
| N0   _  , _         => N0 _
| b       , N0   _    => b
| Npos _ b, Npos _ b' => match PN.sub b b' with
                         | PN.IsPos v => Npos _ v
                         | _          => N0 _
                         end
end .

Definition mul (x: BN) : BN -> BN := starApp (fmap PN.mul x) .

Definition negate (x: BN) : BN := x .

Definition abs (x: BN) : BN := x .

Definition signum (b: BN) : BN :=
match b with 
| N0 _ => N0 _
|  _   => Npos _ PN.XH
end .

Definition fromInteger (n: nat) : BN :=
match n with
| O   => N0   _
| S n => Npos _ (PNI.nat2Pos n)
end . 

(* instance Eq BN *)
Definition eqBN (b b': BN) : bool :=
match b, b' with
|  N0 _    , N0 _      => true
|  Npos _ b, Npos _ b' => PN.eqPositive b b'
|  _       , _         => false
end .

Definition neqBN (x y: BN) : bool := negb (eqBN x y) .

(* instance Ord BN *)
Definition leBN (b b': BN) : bool :=
match b, b' with
| N0   _   , Npos _ _  => true
| Npos _ p , Npos _ p' => PNO.lePositive p p'
|  _       ,  _        => false
end .

Definition leqBN (b b': BN) : bool := 
  orb (leBN b b')  (eqBN b b') .

(* Non passa il Type checking perché la terminazione
non è verificabile banalmente.
*)
(* Fixpoint div (n d: BN) : BN :=
match n with
| N0 _ => N0 _
| _    => if (leBN n d) then N0 _ 
          else succ (div (sub n d) d)
end . 
*)
