From LPP Require Import PositiveNum.
Module PN := PositiveNum.
From LPP Require Import PositiveNumOrder.
Module PNO := PositiveNumOrder.
From LPP Require Import PositiveNumVsInteger.
Module PNI := PositiveNumVsInteger.
From LPP Require Import BNumFuncApp.
Module BNFA := BNumFuncApp.

(** ** Functor laws *)
Proposition BinNotation_fmap_homomorphism: 
  forall {a b c: Set} (f: a -> b) (g: b -> c) (p: BinNotation a),
  ((fmap f) ; (fmap g)) p = fmap (f ; g) p.
Proof. destruct p.
- unfold fmap. reflexivity.
- unfold fmap. reflexivity.
Qed.

(** La dimostrazione è un lambda termine *)
Print BinNotation_fmap_homomorphism. 
(** restituisce:

  > BinNotation_fmap_homomorphism =
  > (fun (a b c : Set) (f : a -> b) (g : b -> c) (p : BinNotation a) =>
  >  match p as b0 return (fmap g (fmap f b0) = fmap (f; g) b0) with
  >  | N0 _ => eq_refl : fmap g (fmap f (N0 a)) = fmap (f; g) (N0 a)
  >  | Npos _ a0 => 
  >	  (fun a1 : a =>
  >      eq_refl : fmap g (fmap f (Npos a a1)) = fmap (f; g) (Npos a a1)) a0
  >  end : forall (a b c : Set) (f : a -> b) (g : b -> c) (p : BinNotation a),
  >        (fmap f; fmap g) p = fmap (f; g) p

che si spiega come segue:
- [(a b c : Set) (f : a -> b) (g : b -> c) (p : BinNotation a) => ...]: definisce 
  i parametri della funzione. Prende tre tipi [a], [b] e [c], insieme a due funzioni 
  [f] e [g]. L'ultimo parametro è [p], un valore di tipo [BinNotation a];

- [match p as b0 return (fmap g (fmap f b0) = fmap (f; g) b0) with ... end]: è un 
  matching sulla struttura di [p: BinNotation a]. La clausola [return] specifica 
  il tipo del in cui [b0] è il nome di [p] su cui si sta applicando il pattern 
  matching;

- [N0 _ => eq_refl : fmap g (fmap f (N0 a)) = fmap (f; g) (N0 a)]: ramo che afferma,
  tramite [eq_refl], costruttore di dimostrazione, che [fmap g (fmap f (N0 a))] e
  [fmap (f; g) (N0 a)] sono uguali.

- [Npos _ a0 => ... (fun a1 : a => eq_refl : fmap g (fmap f (Npos a a1)) = fmap (f; g) (Npos a a1)) a0]: 
  ramo che, per ogni istanza di [a1: BinNOtation a], afferma se [p] ha forma [Npos a a0],
  allora, tramite la dimostrazione eq_refl afferma l'eguaglianza tra  
  [fmap g (fmap f (Npos a a1))] e [fmap (f; g) (Npos a a1)) a0].
*)

Proposition BinNotation_id_preservation: 
  forall (p: BinNotation PN.Positive),
    p = fmap (fun x => x) p.
Proof. 
destruct p.
- unfold fmap. reflexivity.
- unfold fmap. reflexivity.
Qed.

(** *** Instances of Functor laws using [Positive]*)
Corollary BinNotation_fmap_homomorphism_Positive: 
  forall (f g: PN.Positive -> PN.Positive) (p: BinNotation PN.Positive),
  ((fmap f) ; (fmap g)) p = fmap (f ; g) p.
Proof. intros. 
apply (BinNotation_fmap_homomorphism  f g p). Qed.

(* ****************************** *)
(* ****************************** *)
(* ****************************** *)
(* FINE LEZIONE DEL 28/11/23.
   Sono state commentate le 
   tattiche per le dimostrazioni 
   precedenti.                    *)
(* ****************************** *)
(* ****************************** *)
(* ****************************** *)
(* ****************************** *)


(** ESERCIZI *)
Corollary BinNotation_fmap_homomorphism_predSucc:
  forall (p: BinNotation PN.Positive),
    ((fmap PN.pred) ; (fmap PN.succ)) p = fmap (PN.pred ; PN.succ) p.
Admitted. (* Cancellare Admitted. Completare la dimostrazione. *) 
(* Proof. apply (... ). Qed. *)

(** ** Applicative laws *)

(** [fmap] può essere definita come composizione di [<*>] e [pure].
Permette di distribuire l'applicazione di un funtore su tipi freccia
di funzioni con arietà arbitraria. *)
Lemma fmapIsStarPure: forall (a b: Set) (f: a -> b) (x: BinNotation a),
  fmap f x =  (pure f) <*> x.
Admitted. (* Cancellare Admitted. Completare la dimostrazione. *) 
(* 
Proof. destruct x.
...
Qed. 
*)

Proposition BinNotation_fmapVsPure: 
  forall {a b: Set} (f: a -> b) (x: a),
    (pure ; (fmap f)) x = (f ; pure) x.
Admitted. (* Cancellare Admitted. Completare la dimostrazione. *) 

(** Applicando [pure] alla composizione di funzioni, l'applicazione [<*>] 
è associativa. *)
Proposition starComposition: 
  forall (a b c: Set) (g: BinNotation (b -> c)) (f: BinNotation (a -> b)) (x: BinNotation a),
  pure (fun g f => f ; g) <*> g <*> f <*> x = g <*> (f <*> x).
Admitted. (* Cancellare Admitted. Completare la dimostrazione.
Ovviamente non è necessario seguire la struttura suggerita. *) 
(* 
Proof. 
unfold pure. 
destruct x.
- ...
  + ...
  + ...
    * ...
    * ...
- ...
  + ...
  + ...
    * ...
    * ...
Qed. *)

(** **** [<*>] e [pure] si "scambiano" rispetto a [<*>] *)
Proposition starInterchange: forall (a b: Set) (g: BinNotation (a -> b)) (x: a),
g <*> (pure x) =  (pure ((fun f => f x):(a -> b) -> b)) <*> g.
Admitted. (* Cancellare Admitted. Completare la dimostrazione. *) 
