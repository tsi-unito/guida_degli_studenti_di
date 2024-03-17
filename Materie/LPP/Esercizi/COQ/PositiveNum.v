Module PositiveNum.

(** ** Tipo di dato [Positive]*)
Inductive Positive: Set :=
    | XI : Positive -> Positive
    | XO : Positive -> Positive
    | XH : Positive .

(** **** Principio di induzione per [Positive]*)
Check Positive_ind.
(** : forall P : Positive -> Prop,
       (forall p : Positive, P p -> P (XI p)) ->
       (forall p : Positive, P p -> P (XO p)) ->
       P XH -> forall p : sPositive, P p 

C'è anche quello per i naturali:
*)
Check nat_ind.
(** : forall P : nat -> Prop,
       P 0 -> (forall n : nat, P n -> P (S n)) 
              -> forall n : nat, P n *)
(** [] *)

(** ** Operazioni su [Positive] *)
Fixpoint succ (p: Positive) : Positive :=
match p with
| XH   => XO XH
| XO b => XI b
| XI b => XO (succ b)
end. 

Fixpoint pred (p: Positive) : Positive :=
match p with  
| XH     => XH
| XO XH  => XH
| XI b   => XO b
| XO b   => XI (pred b)
end.

Fixpoint add (p p': Positive) : Positive :=
match p, p' with
| XH,     XH      => XO XH
| XH,     (XO n)  => XI n
| XH,     (XI n)  => XO (succ n)
| (XO m), XH      => succ (XO m)
(* 2m+1 + 1 == 2m + 2 == 2(m + 1) *)
| (XI m), XH      => XO (succ m)
(* 2m + 2n == 2(m + n) *)
| (XO m), (XO n) => XO (add m n)
(* 2m + 2n+1 == 2(m + n) + 1 *)
| (XO m), (XI n) => succ (XO (add m n))
(* 2m+1 + 2n+1 == (2m + 2n)+2 == 2((m + n)+1) *)
| (XI m), (XI n) => XO (succ (add m n))
(* 2m+1 + 2n == 2(m + n)+1 *)
| (XI m), (XO n) => succ (XO (add m n))
end.

Fixpoint mul (p p': Positive) : Positive :=
match p, p' with
| XH    , y => y
| (XO b), y =>        XO (mul b y)
| (XI b), y => add y (XO (mul b y))
end.

Fixpoint mul' (p p': Positive) : Positive :=
match p, p' with
| XH, y  => y
| y , XH => y
| XO m, XO n => XO (XO (mul' m n))
| XO m, XI n => XO (add (XO (mul' m n)) m)
| XI m, XO n => XO (add (XO (mul' m n)) n)
| XI m, XI n => XI (add (add (XO (mul' m n)) m) n)
end.

Fixpoint eqPositive (p p': Positive) : bool :=
match p, p' with
|  XH   ,  XH     => true
| (XO b), (XO b') => eqPositive b b'
| (XI b), (XI b') => eqPositive b b'
|  _    ,  _      => false
end.

Definition neqPositive (p p': Positive) : bool :=
  negb (eqPositive p p') .

Inductive Mask : Set :=
 | IsNeg : Mask
 | IsNul : Mask
 | IsPos : Positive -> Mask .

Fixpoint sub (p p': Positive) : Mask :=
match p, p' with
| XH,     XH      => IsNul
| XH,     _       => IsNeg
| m ,     XH      => IsPos (pred m)
(* 2m - 2n == 2(m - n) *)
| XO m, XO n => match sub m n with
                | IsNeg => IsNeg 
                | IsNul => IsNul
                | IsPos s => IsPos (XO s)
                end
(* (2m+1) - 2n == 2(m - n) + 1 *)
| XI m, XO n => match sub m n with
                | IsNeg => IsNeg 
                | IsNul => IsPos XH
                | IsPos s => IsPos (succ (XO s))
                end
(* (2m) - (2n+1) == 2 (m - n) - 1   *)
| XO m, XI n => match sub m n with
                | IsNeg => IsNeg 
                | IsNul => IsNeg
                | IsPos s => IsPos (pred (XO s))
                end
(* (2m+1) - (2n+1) == 2(m - n)  *)
| XI m, XI n => match sub m n with
                | IsNeg => IsNeg 
                | IsNul => IsNul
                | IsPos s => IsPos (XO s)
                end
end.

(** * Proprietà delle operazioni aritmetiche su [Positive].
Sorgente:
https://github.com/coq/coq/blob/master/theories/PArith/BinPos.v .
*)


(** ** Proprietà di [succ] e [pred] *)

(** *** [XI] caratterizzato da [succ] e [XO] *)
(** Assunzione n.ro 3 nell'elenco delle assunzioni 
usate per progettare (almeno) le funzioni [succ] 
e [pred] *)
Lemma xI_succ_xO p : XI p = succ (XO p).
Proof. destruct p.
- simpl. reflexivity. 
- simpl. reflexivity. 
- simpl. reflexivity.
Qed.  

Lemma succ_discr p : p <> succ p.
Proof. unfold not. destruct p. 
- intros. discriminate H.
- intros. discriminate H.
- intros. discriminate H.
Qed.  

(** Assunzione n.ro 4 nell'elenco delle assunzioni 
usate per progettare (almeno) le funzioni [succ] 
e [pred] *)
Lemma pred_XOsucc: forall (p: Positive),
  XI p = pred (XO (succ p)).
Proof. induction p as [ p' HI | p'' HO | ].
3: { simpl. reflexivity. }
- unfold succ. rewrite HI. fold succ. 
  simpl. reflexivity.
- simpl. reflexivity. 
Qed.

(** *** [succ] e "il predecessore del doppio" *)

Fixpoint pred_double x :=
  match x with
  (* 2(2p+1)-1 == 4p+1 == succ(XO(XO p)) == XI(XO p) *)
    | XI p => XI (XO p)
    | XO p => XI (pred_double p)
    | XH => XH
  end.

Lemma pred_double_spec p : 
  pred_double p = pred (XO p).
Proof. induction p.
3:{ simpl. reflexivity. }
- simpl. reflexivity.
- simpl. rewrite IHp. simpl. reflexivity.
Qed.

End PositiveNum.