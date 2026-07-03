From LPP Require Import PositiveNum.
Module PN := PositiveNum.
Require Import Arith.
Require Import Nat.
Require Import Lia.

(** ** Conversioni tra [Positive] e [nat] *)
Fixpoint pos2Nat (p: PN.Positive) : nat :=
match p with
| PN.XH   => 1
| PN.XO p' => 2 * pos2Nat p'
| PN.XI p' => 1 + 2 * pos2Nat p'
end .

(** https://github.com/coq/coq/blob/master/theories/PArith/BinPosDef.v
*)
Fixpoint nat2Pos (n: nat) : PN.Positive :=
match n with
| O    => PN.XH 
| S O  => PN.XH (* base *)
| S n' => PN.succ (nat2Pos n')
end .


(** **** Test *)
(* 
Example nat2PosO: nat2Pos O = PN.XH.
Proof. auto. Qed.
Example nat2Pos1: nat2Pos 1 = PN.XH.
Proof. auto. Qed.
Example nat2Pos2: nat2Pos 2 = PN.XO PN.XH.
Proof. unfold nat2Pos. unfold even. reflexivity. Qed.
Example nat2Pos3: nat2Pos 3 = PN.XI PN.XH.
Proof. unfold nat2Pos. unfold even. reflexivity. Qed.
Example nat2Pos4: nat2Pos 4 = PN.XO (PN.XO PN.XH).
Proof. unfold nat2Pos. unfold even. simpl. reflexivity. Qed.
Example nat2Pos5: nat2Pos 5 = PN.XI (PN.XO PN.XH).
Proof. unfold nat2Pos. unfold even. simpl. reflexivity. Qed.
Example nat2Pos6: nat2Pos 6 = PN.XO (PN.XI PN.XH).
Proof. auto. Qed.
Example nat2Pos7: nat2Pos 7 = PN.XI (PN.XI PN.XH).
Proof. auto. Qed. 
*)
(* 
Example pos2Nat1: pos2Nat PN.XH = 1.
Proof. auto. Qed.
Example pos2Nat2: pos2Nat (PN.XO PN.XH) = 2.
Proof. auto. Qed.
Example pos2Nat3: pos2Nat (PN.XI PN.XH) = 3.
Proof. auto. Qed.
Example pos2Nat4: pos2Nat (PN.XO (PN.XO PN.XH)) = 4.
Proof. auto. Qed. *)

(** ** Relazioni tra [Positive] e [nat] *)

(** *** Da [nat] a [Positive] *)

(* BinPos.Pos.of_nat_succ:
  forall n : nat, BinPos.Pos.of_succ_nat n
                    = BinPos.Pos.of_nat (S n) *)
Lemma succPos2Nat_nat2PosS_1:
  PN.succ (nat2Pos 1) = nat2Pos (S 1).
Proof. unfold nat2Pos. simpl. reflexivity. Qed. 

Lemma succPos2Nat_nat2PosS_atLeast2: forall (n: nat),
  PN.succ (nat2Pos (S n)) = nat2Pos (S (S n)).
Proof. destruct n.
- simpl. reflexivity. 
- unfold nat2Pos. fold nat2Pos. reflexivity.
Qed. 

(* Pnat.Nat2Pos.inj_0: BinPos.Pos.of_nat 0 = BinNums.xH *)
Lemma Nat2Positive_0: nat2Pos 0 = PN.XH.
Proof. auto. Qed.

Lemma Nat2Positive_1: nat2Pos 1 = PN.XH.
Proof. auto. Qed.

Lemma Nat2Positive_SSn: forall (n: nat),
  exists (p: PN.Positive), nat2Pos (S (S n)) = PN.XO p 
                           \/ nat2Pos (S (S n)) = PN.XI p.
Proof. induction n as [ | n' IH].
- exists PN.XH. left. simpl. reflexivity.
- destruct IH.
  destruct H as [Hl | Hr].
  unfold nat2Pos. fold nat2Pos.
  + exists x.
    left. rewrite <- Hl. 
    simpl. 
    (* reflexivity.  <----- NOK!!! *)
Admitted.

(** *** Da [Positive] a [nat] *)
Lemma Positive2Nat_XH: 1 = pos2Nat PN.XH.
Proof. unfold pos2Nat. reflexivity. Qed.

(* Pnat.Pos2Nat.inj_xO:
  forall p : BinNums.positive,
  BinPos.Pos.to_nat (BinNums.xO p) = 2 * BinPos.Pos.to_nat p. *)
Lemma Positive2Nat_XO: forall (p: PN.Positive),
2 * (pos2Nat p) = (pos2Nat (PN.XO p)).
Proof. destruct p.
- simpl. reflexivity.
- simpl. reflexivity.
- simpl. reflexivity.
Qed.

(* Pnat.Pos2Nat.inj_xI:
  forall p : BinNums.positive,
  BinPos.Pos.to_nat (BinNums.xI p) = S (2 * BinPos.Pos.to_nat p). Pnat.Pos2Nat.id:
  forall p : BinNums.positive, BinPos.Pos.of_nat (BinPos.Pos.to_nat p) = p *)
Lemma Positive2Nat_XI: forall (p: PN.Positive),
S (2 * (pos2Nat p)) = (pos2Nat (PN.XI p)).
Proof. destruct p.
- simpl. reflexivity.
- simpl. reflexivity.
- simpl. reflexivity.
Qed.

Lemma succXO_is_XI: forall (p: PN.Positive),
  PN.succ (PN.XO p) = PN.XI p.
Proof. unfold PN.succ. reflexivity. Qed.

Lemma predXOSucc_is_XI  : forall (p: PN.Positive),
  PN.pred (PN.XO (PN.succ p)) = PN.XI p.
Proof. induction p.
- unfold PN.succ. fold PN.succ. 
  unfold PN.pred. fold PN.pred.
  rewrite <- IHp. 
  simpl. reflexivity.
- simpl. reflexivity.
- simpl. reflexivity.
Qed.

Proposition pos2Nat_is_succ: forall (p: PN.Positive), 
  exists (n: nat), pos2Nat p = S n.
Proof. induction p.
3: { unfold pos2Nat. exists 0. reflexivity. }
- destruct IHp. unfold pos2Nat. fold pos2Nat. 
  rewrite H. exists (2*(S x)). reflexivity.
- destruct IHp. unfold pos2Nat. fold pos2Nat. 
  rewrite H. exists (S (2*x)).
  unfold mul. simpl. 
  Search (_ + S _ = S _).
  rewrite Nat.add_succ_r. (* n + S m = S (n + m) *)
  reflexivity.
Qed.

(** *** Lemmi propedeutici per [pos2Nat_nat2Pos]*)
Lemma pos2NatSucc_Spos2Nat_XH:
  pos2Nat (PN.succ PN.XH) = S (pos2Nat PN.XH).
Proof. unfold PN.succ. unfold pos2Nat. auto. Qed.

Lemma pos2NatSucc_Spos2Nat_XO: forall (p: PN.Positive),
  pos2Nat (PN.succ p) = S (pos2Nat p)
  -> pos2Nat (PN.succ (PN.XO p)) = S (pos2Nat (PN.XO p)).
Proof. intros. simpl. reflexivity. Qed.

Lemma pos2NatSucc_Spos2Nat_XI: forall (p: PN.Positive),
  pos2Nat (PN.succ p) = S (pos2Nat p)
  -> pos2Nat (PN.succ (PN.XI p)) = S (pos2Nat (PN.XI p)).
Proof. intros.
simpl. rewrite H. simpl.
Search (_ + 0) . (* Nat.add_0_r: forall n : nat, n + 0 = n *)
rewrite Nat.add_0_r.
Check f_equal. 
apply f_equal.
Search (_ + S _ = S _). 
(* Nat.add_succ_r: forall n m : nat, n + S m = S (n + m) *)
rewrite Nat.add_succ_r.
reflexivity.
Qed.

(** *** Proprietà [pos2Nat_nat2Pos] *)
(** Assicura che il successore su [PN.Positive] corrisponde al 
successore su [nat]. 

La dimostrazione della proprietà principale 
[pos2Nat_nat2Pos_statement] definita qui sotto è 
sviluppata applicando il principio di induzione
[PN.Positive_ind] associato a al tipo induttivo
[PN.Positive].   *)
Definition pos2Nat_nat2Pos_statement (p: PN.Positive) :=
pos2Nat (PN.succ p) = S (pos2Nat p).

Check pos2Nat_nat2Pos_statement.
Check (PN.Positive_ind pos2Nat_nat2Pos_statement).
Check (PN.Positive_ind 
        pos2Nat_nat2Pos_statement
        pos2NatSucc_Spos2Nat_XI
        pos2NatSucc_Spos2Nat_XO
        pos2NatSucc_Spos2Nat_XH). 

Proposition pos2Nat_nat2Pos: forall (p: PN.Positive),
  pos2Nat (PN.succ p) = S (pos2Nat p).
Proof
PN.Positive_ind 
        pos2Nat_nat2Pos_statement
        pos2NatSucc_Spos2Nat_XI
        pos2NatSucc_Spos2Nat_XO
        pos2NatSucc_Spos2Nat_XH. 

(** *** Lemmi propedeutici per [nat2Pos_pos2Nat]*)


(* DA QUI!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* DA QUI!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* DA QUI!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)


(** DA FARE *)  
(* 
Search ( BinPos.Pos.of_nat _).
Pnat.Pos2Nat.id:
  forall p : BinNums.positive, BinPos.Pos.of_nat (BinPos.Pos.to_nat p) = p

BinPos.Pos.pred_of_succ_nat:
  forall n : nat,
  BinPos.Pos.pred (BinPos.Pos.of_succ_nat n) = BinPos.Pos.of_nat n
Pnat.Nat2Pos.inj_pred:
  forall n : nat,
  BinPos.Pos.of_nat (pred n) = BinPos.Pos.pred (BinPos.Pos.of_nat n)
Pnat.Nat2Pos.id:
  forall n : nat, n <> 0 -> BinPos.Pos.to_nat (BinPos.Pos.of_nat n) = n
*)



(** *** Relazioni tra [pos2Nat] e operazioni binarie su [Positive] *)
(* Pnat.Pos2Nat.inj_add:
  forall p q : BinNums.positive,
  BinPos.Pos.to_nat (BinPos.Pos.add p q) =
  BinPos.Pos.to_nat p + BinPos.Pos.to_nat q *)
Lemma pos2Nat_inj_add: forall p q : PN.Positive,
  pos2Nat (PN.add p q) = pos2Nat p + pos2Nat q.
Admitted.

(* Pnat.Pos2Nat.inj_pred:
  forall p : BinNums.positive,
  BinPos.Pos.lt BinNums.xH p ->
  BinPos.Pos.to_nat (BinPos.Pos.pred p) = Nat.pred (BinPos.Pos.to_nat p) *)
(* Lemma pos2Nat_inj_pred: forall (p: PN.Positive),
  PN.lt PN.xH p ->
  pos2Nat (PN.pred p) = PN.pred (pos2Nat p).
Admitted. *)


(** *** Altre proprietà *)

Lemma zeroSmallerThanSn: forall n: nat,
  0 < S n -> 0 < S(S n).
Proof. intros.
Check (Nat.lt_trans 0 (S n) (S (S n))).
apply (Nat.lt_trans 0 (S n) (S (S n))). 
- assumption. 
- Search ( S _ < S _).
  apply lt_n_S. (* n < m -> S n < S m *)
  Search ( _ < S _).
  apply Nat.lt_succ_diag_r. (* n < S n *)
Qed.
