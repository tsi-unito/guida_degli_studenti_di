Require Import String. 
Require Import Ascii. 
Require Import List.  
Import Notations ListNotations.

Module MonadException. 

Notation "f ; g" :=
  (fun x =>  g (f x))
  (at level 60, right associativity) .
  (* : scope. *)

(** * Tipo di dato astratto *)
(** Per definizione, fissato un insieme [a], costruisce un 
nuovo insieme in cui gli elementi hanno forma [Rise "qualche msg"]
o, per un qualsiasi elemento [x:a] fissato, [Return x] *)

Definition Exception := string.
Inductive E ...

    
(** * E è un funtore *)
(** È possibile definire le funzioni:
    - [id] che funge da identità;
    - [fmap] che, ad ogni morfismo [f: a -> b], associa un 
    morfismo di tipo [E a -> E b].
*)
Definition id ...
      
Definition fmap ...
  
(** [id] e [fmap: E a -> E b] soddisfano le due proprietà 
funtoriali: *)
Lemma idIsIdentity: ... 

Lemma fmapComposes: ...

(** * E è anche un funtore "Applicative" *)
(** Significa che [E] ammette le funzioni [pure] e [<*>] 
che soddisfano specifiche proprietà che lo rendono una 
struttura "Applicative". *)

(** **** Funzione [pure] *)
(** Incapsula un qualsiasi valore in [E]: *)
Definition pure ...

(** **** Funzione [<*>] *)
(** Applica (da qui il nome "Applicative"?) una funzione 
incapsulata ad un valore incapsulato: *)
Definition star ...

Notation "x <*> y" :=
  (star x y)
  (at level 60, right associativity) .  (* in BNumFuncApp 
                                           è left 
                                           ASSOCIATIVE! *)
  (* : scope. *)

(** ** Proprietà di [pure] e [<*>] *)
(** Sono le proprietà per cui possiamo affermare che [E] 
è un funtore applicativo. Sono elencate una ad una qui di 
seguito e dimostrate.
*)

(** **** [pure] è un Omomorfismo rispetto a [<*>] *)
(** Usare [<*>] per applicare una funzione a un valore,
entrambi incassati in [E] tramite [pure], equivale ad 
applicare la funzione all'argomento, per poi incassare 
in [ID] il risultato. *)
Proposition pureHomomorphic: ...

(** **** [pure] e [<*>] soddisfano l'Identità applicativa *)
(** L'Identità applicativa afferma che, usando [<*>] per 
applicare un'identità [id: a -> a], inglobata da [pure], 
ad un qualunque argomento, il comportamento di [id: a -> a]
non è alterato da [pure]: *)
Proposition pureIdentity: ...

(** **** Legge della composizione/associatività *)
(* Applicando [pure] alla composizione di funzioni, 
l'applicazione [<*>] è associativa. *)
Proposition starComposition: ...

(** **** [<*>] e [pure] si "scambiano" rispettto a [<*>] *)
Proposition starInterchange: ...

(** ** [fmap] di un funtore "Applicative" *)
(** [fmap] può essere definita come composizione di [<*>] e [pure]  *)
Lemma fmapIsStarpure: ...

(** * E è un è anche una Monade *)
(** Per il funtore [E] è possibile definire le funzioni [Return] e 
[bind] che soddisfano specifiche proprietà. *)

(** **** Funzione [Return] *)
Definition returN ...

(** **** Funzione [bind] *)
Definition bind ...

Notation "x >>= y" :=
  (bind x y)
  (at level 60, right associativity) .
  (** : scope. *)
  
(** ** Proprietà di [Return] e [bind] *)
Proposition returNLeftIdentity: ...

Proposition returnRightIdentity: ...

Proposition bindAssoc: ...
  
(** ** Relazioni tra strutture "Functor", "Applicative" e "Monad" *)
(**  Le operazioni associate alle due strruture 
menzionate devono relazionarsi come segue.

Da un lato [pure] e [returN] devono essere funzioni 
equivalenti:
*)
Proposition pureEqreturN: ...

(** Dall'altro, la composizione "Applicative" [<*>] deve 
essere definibile per mezzo delle due funzioni [returN] e [>>=] 
([bind]) che caratterizzano la monade: *)
Proposition starEqBindreturN: ...

(** Anche la funzione [fmap] del funtore alla base della monade è 
definibile tramite [returN] e [>>=] ([bind]) *)
Proposition fmapEqBindReturn: ...

End MonadException.