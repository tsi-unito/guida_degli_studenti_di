From LPP Require Import PositiveNum.
Module PN := PositiveNum.

(* propedeutico alla definizione di (<):: BNum -> BNum -> Bool *)
Inductive Comp: Set :=
|  Lt : Comp
|  Eq : Comp
|  Gt : Comp .

Fixpoint comp (p p': PN.Positive) : Comp :=
match p, p' with
| PN.XH   , PN.XH    => Eq
| PN.XH   , PN.XO _  => Lt
| PN.XH   , PN.XI _  => Lt
| PN.XO _ , PN.XH    => Gt
| PN.XI _ , PN.XH    => Gt
| PN.XO p , PN.XO p' => comp p p'
| PN.XI p , PN.XI p' => comp p p'
| PN.XO p , PN.XI p' => match (comp p p') with
                     | Lt => Lt
                     | Eq => Lt
                     | Gt => Gt
                     end
| PN.XI p, PN.XO p' => match comp p p' with
                     | Lt => Lt
                     | Eq => Gt
                     | Gt => Gt
                     end
end .

Definition lePositive (p p': PN.Positive) : bool :=
match comp p p' with
| Lt => true
| _  => false
end .

Definition leqPositive (p p': PN.Positive) : bool :=
  orb (lePositive p p') (PN.eqPositive p p').
