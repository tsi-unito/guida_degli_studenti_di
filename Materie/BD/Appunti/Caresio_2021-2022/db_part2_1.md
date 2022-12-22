---
title: "Basi di dati - Parte II - Calcolo relazionale"
---

# Basi di dati - Parte II

## Calcolo relazionale

- L'algebra relazione è un linguaggio di tipo **procedurale**.
    - Dove l'utente indica le operazioni da compiere per arrivare al risultato e il DBMS sceglie poi la strategia ottimale.
- Il **calcolo relazione** interroga le basi di dati invece con un approccio **dichiarativo** fondato sulla logica:
    - Si specificano proprietà del risultato anziché la procedura per generarlo.
- Verrà trattato però il solo **calcolo relazionale su tuple con dichiarazione di range**, base teorica di SQL, e non il **calcolo su domini**.
    - *Su tuple* perché le variabili denotano **tuple**;
    - *Con dichiarazione di range* perché permette di specificare il range di valori che le variabili possono assumere.

### Interrogazioni in calcolo relazionale

- L'interrogazione è composta da tre parti $\{ T \: | \: L \: | \: F \}$:
    - $Target$ ($T$): specifica quali attributi compaiono nel risultato;
    - $Range \: list$ ($L$): specifica il dominio dalle variabili non quantificate in $F$;
    - $Formula$ ($F$): specifica una formula logica che il risultato deve soddisfare.
- Il risultato di un'interrogazione è dato dall'insieme dei valori degli attributi $T$ delle tuple nelle variabili in $L$ che rispettano la formula in $F$.
    - Si tratta quindi di una relazione virtuale.

#### Range list

- La **range list** non è altro che l'introduzione di variabili abbinate a relazioni di base.
    - Della forma $nomeVariabile(NomeRelazioneDiBase)$.
        - $nomeVariabile$ assume valori nella relazione $nomeRelazioneDiBase$, cioè è una sua qualunque tupla.
    - Si tratta di relazioni di base (già presenti nel DB) e quindi non di relazioni virtuali.

#### Formula

- La **formula** è un predicato del primo ordine che vincola le variabili della range list.
    - I predicati di base (atomi) sono del tipo:
        - $x.A_i \: \varphi \: costante$;
        - $x.A_i \: \varphi \: y.A_j$.
        - Dove $x$ e $y$ sono delle variabili, $A_i$ e $A_j$ sono degli attributi e $\varphi$ è un operatore di confronto.
    - Il predicato è costruito con gli operatori $\{ \land, \lor, \lnot, \Rightarrow \}$ e dai quantificatori $\{ \exists, \forall \}$.

#### Target list

- La **target list** è l'elenco delle informazioni volute in uscita.
    - Le variabili usate nella target list **devono essere dichiarate nella range list**.
    - Nei predicati con quantificatori possono essere contenute sia variabili libere che vincolate.
        - Tuttavia tutte le variabili libere presenti nella formula devono essere dichiarate nella range list.
    - La target list, quindi, utilizza **solo variabili libere**.
- Sintassi possibile della target list:
    - $variabile.Attributo1, variabile.Attributo2, \dots$;
    - $variabile.(Attributo1, Attributo2)$;
    - $variabile.*$;
    - $Nome: variabile.Attributo$.

### Operazioni

- Negazione esistenziale: $\{T \: | \: L \: | \: formulaU \land \lnot formulaP \}$.
- Prodotto cartesiano: $\{x.*, y.* \: | \: x(R), y(S) \}$.

#### Quantificazione esistenziale e universale

- Sintassi della quantificazione esistenziale e universale nel calcolo relazionale:
    - $\exists \: variabile \: (Relazione) \: (formula)$;
    - $\forall \: variabile \: (Relazione) \: (formula)$.
- La formula è un predicato del primo ordine che può contenere sia variabili libere che quantificate (vincolate).
    - Tutte le variabili libere presenti nella formula devono essere dichiarate nella range list.

### Limiti del calcolo relazionale

- L'unione insiemistica dell'algebra relazionale **non è esprimibile** col calcolo relazionale su tuple con dichiarazione di range.
    - Infatti l'unione dovrebbe trarre le tuple dalle due relazioni da unire, che dovrebbero quindi comparire nella range list.
    - Manca inoltre la ricorsione, ma si può implementare una visita in profondità.
        - Purché il livello di profondità sia **determinato a priori**.
        - `eg` Albero genealogico, è necessario definire il numero di self-join per accedere a determinato antenato.

#### Calcolo relazionale e SQL

- Il calcolo relazionale con dichiarazione di range ha ispirato direttamente il SQL.
    - La target list corrisponde alla `SELECT`;
    - La range list corrisponde alla `FROM`;
    - La formula corrisponde alla `WHERE`.
- L'unione non è presente direttamente nel costrutto SQL `SELECT ... FROM ... WHERE`.
    - Ma è espressa fuori dal costrutto con `UNION`.
- Le versioni di SQL più moderne implementano una forma di ricorsione attraverso le chiusure transitive.
