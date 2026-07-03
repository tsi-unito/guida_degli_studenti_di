Module MonadID. 

Notation "f ; g" :=
  (fun x =>  g (f x))
  (at level 60, right associativity) .
  (* : scope. *)

(** * Tipo di dato astratto ID *)
(** Per definizione, fissato un insieme [a], associa a ogni 
oggetto [x: a] un oggetto [Id x: ID a] *)
Inductive ID (a: Set) : Set :=
  | Id : a -> ID a .
    
(** * ID è un funtore *)
(** È possibile definire le funzioni:
    - [id] che funge da identità;
    - [fmap] che, ad ogni morfismo [f: a -> b], associa un 
    morfismo di tipo [ID a -> ID b].
*)
Definition id ...
      
Definition fmap ...
  
(** [id] e [fmap: ID a -> ID b] soddisfano le due 
proprietà funtoriali: *)
Lemma idIsIdentity: ...
Lemma fmapComposes: ...

(** * ID è un è anche un funtore "Applicative" *)
(** Significa che [ID] ammette le funzioni [pure] e [<*>] 
che soddisfano specifiche proprietà che lo rendono una 
struttura "Applicative". *)

(** **** Funzione [pure] *)
(** Incapsula un qualsiasi valore in [ID]: *)
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
(** Sono le proprietà per cui possiamo affermare che [ID] 
è un funtore applicativo. Sono elencate una ad una qui di 
seguito e dimostrate.
*)

(** **** [pure] è un Omomorfismo rispetto a [<*>] *)
(** Usare [<*>] per applicare una funzione a un valore, 
entrambi incassati in [ID] tramite [pure], equivale ad applicare 
la funzione all'argomento, per poi incassare in [ID] il 
risultato. *)
Proposition pureHomomorphic: ...

(** **** [pure] e [<*>] soddisfano l'Identità applicativa *)
(** L'Identità applicativa afferma che, usando [<*>] per 
applicare un'identità [id: a -> a], inglobata da [pure], 
ad un qualunque argomento, il comportamento di [id: a -> a] 
non è alterato da [pure]: *)
Proposition pureIdentity: forall (a: Set) (x: ID a),
  (pure (fun x => x)) <*> x = x.
Proof. unfold pure. unfold star. destruct x. reflexivity.
Qed.
(** NOTA. [pureIdentity] non è esattamente un corollario 
di [pureHomomorphic]. In [pureIdentity] l'argomento di 
[pure f] è certamente di tipo [ID a], ma non è necessariamente
ottenuto applicando [pure]. *)

(** **** Legge della composizione/associatività *)
(* Applicando [pure] alla composizione di funzioni, 
l'applicazione [<*>] è associativa. *)
Proposition starComposition: ...

(** **** [<*>] e [pure] si "scambiano" rispettto a [<*>] *)
Proposition starInterchange: ...

(** ** [fmap] di un funtore "Applicative" *)
(** [fmap] può essere definita come composizione di [<*>] 
e [pure] *)
Lemma fmapIsStarpure: ...

(** * ID è un è anche una Monade *)

(** Per il funtore [ID] è possibile definire le funzioni 
[Return] e [bind] che soddisfano specifiche proprietà. *)

(** **** Funzione [Return] *)
Definition Return ...

(** **** Funzione [bind] *)
Definition bind ...

Notation "x >>= y" :=
  (bind x y)
  (at level 60, right associativity) .
  (** : scope. *)
  
(** ** Proprietà di [Return] e [bind] *)
Proposition ReturnLeftIdentity: ...

Proposition ReturnRightIdentity: ...
  
Proposition bindAssoc: ...
  

(** ** Relazioni tra strutture "Functor", "Applicative" e "Monad" *)
(** Le operazioni associate alle due strruture menzionate 
devono relazionarsi come segue.

Da un lato [pure] e [Return] devono essere funzioni 
equivalenti:
*)
Proposition pureEqReturn: ...

(** Dall'altro, la composizione "Applicative" [<*>] deve 
essere definibile per mezzo delle due funzioni [Return] 
e [>>=] ([bind]) che caratterizzano la monade: *)
Proposition starEqBindReturn: ...

(** Anche la funzione [fmap] del funtore alla base della 
monade è definibile tramite [Return] e [>>=] ([bind]) *)
Proposition fmapEqBindReturn: ...

End MonadID.
