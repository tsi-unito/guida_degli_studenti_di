---
title: "Basi di dati - Parte II - Esercizi - Calcolo relazionale"
---

# Basi di dati - Parte II - Esercizi

## Calcolo relazionale

### Esempi introduttivi

- Elencare il nome e il cognome dei pazienti residenti a Torino.
    - $\{p.nome, p.Cognome \: | \: p(Pazienti) \: | \: p.Residenza= \: 'TO' \}$.
- Elencare il cognome, il nome e la data del ricovero dei pazienti ricoverati nel reparto $A$.
    - Si introducono due variabili:
        - $L: \: p(Pazienti), r(Ricoveri)$.
    - È necessario il join tra $pazienti$ e $ricoveri$ e la condizione sui reparti:
        - $F: \: p.COD=r.PAZ \land r.Reparto= \: 'A'$.
    - Si estrae cognome e nome del paziente e data del ricovero:
        - $T: \: p.Cognome, p.Nome, r.Inizio$.
    - Interrogazione completa:
        - $\{p.Cognome, p.Nome, r.Inizio \: | \: p(pazienti), r(Ricoveri) \: | \: p.COD=r.PAZ \land r.Reparto = \: 'A' \}$.

#### Quantificazione esistenziale

- Elencare il cognome e il cognome dei pazienti ricoverati nel reparto $A$.
    - Non sono richieste informazioni riguardo al ricovero.
    - Si può riformulare l'interrogazione utilizzando la **quantificazione esistenziale**.
  - $\{p.Cognome, p.Nome \: | \: p(pazienti) \: | \: \exists r(Ricoveri) (p.COD=r.PAZ \land r.Reparto = \: 'A') \}$.
- Elencare il cognome e il nome dei medici che curano il paziente A102.
    - $L: \: m(medici)$;
    - $T: \: m.Cognome, m.Nome$;
    - $F: \: \exists r(Ricoveri)(r.Reparto=m.Reparto \land r.PAZ= \: 'A102')$.
- Elencare il nome e il cognome dei medici che curano il paziene Piero Rossini.
    - $L: \: m(medici)$;
    - $T: \: m.Cognome, m.Nome$;
    - $F: \exists p(Pazienti)(\exists r (Ricoveri)(A \land B \land C))$.
        - $A = P.COD=R.PAZ$;
        - $B = r.Reparto = m.Reparto$;
        - $C = p.Nome = \: 'Piero' \land p.Cognome= \: 'Rossini'$.

#### Intersezione

- Elencare cognome e nome comuni a pazienti e medici.
    - Realizza quello che nell'algebra relazione realizza l'intersezione.
    - $L: \: p(Pazienti)$;
    - $T: \: p.Cognome, p.Nome$;
    - $F: \: \exists m(Medici)(m.Cognome=p.Cognome \land m.Nome = p.Nome)$.
    - O simmetricamente:
        - $L: \: m(Medici)$;
        - $T: \: m.Cognome, m.Nome$;
        - $F: \: \exists p(Pazienti)(p.Cognome=m.Cognome \land p.Nome = m.Nome)$.

#### Negazione

- Elencare i cognomi e nomi dei pazienti **non** comuni con i medici.
    - In Algebra relazionale si risolverebbe utilizzando l'operatore di differenza.
    - $L: \: p(Pazienti)$;
    - $T: \: p.Cognome, p.Nome$;
    - $F: \: \lnot \exists m(Medici)(m.Cognome=p.Cognome \land m.Nome = p.Nome)$.
    - Applicando De Morgan:
        - $F: \: \forall m(Medici)(m.Cognome \neq p.Cognome \lor m.Nome \neq p.Nome)$.

#### Quantificazione universale

- Elencare i pazienti con almeno un ricovero in ogni reparto.
    - $L: \: p(Pazienti)$;
    - $T: \: p.*$;
    - $F: \: \forall r(Reparti)(\exists r'(Ricoveri)(r'.Reparto=r.COD \land r'.PAZ = p.COD))$.
    - Per ogni istanza in $r$ **deve esistere** un ricovero di quel paziente nel reparto $r$.
    - Alternativa con doppia negazione:
        - $F: \: \lnot \lnot \forall r(Reparti)(\exists r'(Ricoveri)(r'.Reparto=r.COD \land r'.PAZ = p.COD))$;
        - $F: \: \lnot \exists r(Reparti)(\lnot \exists r'(Ricoveri)(r'.Reparto=r.COD \land r'.PAZ = p.COD))$.
        - Non esiste un oggetto che non ha quella proprietà.
        - In SQL l'interrogazione si esprime esattamente così.
            - Non esiste la quantificazione universale ma quella esistenziale e il not.

#### Conteggio

- Elencare i pazienti ricoverati duo o più volte.
    - In algebra relazionale si userebbe probabilmente il self-join.
        - Nel calcolo relazionale si utilizza una tecnica simile.
    - $L: \: p(Pazienti)$;
    - $T: \: p.*$;
    - $F: \: \exists r'(Ricoveri)(\exists r''(Ricoveri)(p.COD=r'.PAZ \land p.COD=r''.PAZ \land r'Inizio \neq r''.Inizio))$.
- Elencare i pazienti ricoverati una sola volta.
    - Equivalente a elencare i pazienti ricoverati ma che non sono stati ricoverati due o più volte.
    - $L: \: p(Pazienti)$;
    - $T: \: p.*$;
    - $F: \: \lnot \exists r'(Ricoveri)(\exists r''(Ricoveri)(p.COD=r'.PAZ \land p.COD=r''.PAZ \land r'Inizio \neq r''.Inizio))$.

#### Negazione esistenziale

- Elencare i medici non primari.
    - Si risolve come $\{T \: | \: L \: | \: formulaU \land \lnot formulaP \}$.
    - $L: \: m(Medici)$;
    - $T: \: m.*$;
    - $F: \: \lnot \exists r(Reparti)(r.Primario = m.MATR)$.
    - L'universo $U$ è definito implicitamente nella variabile libera $m$.
- Elencare gli studenti che hanno sostenuto tutti gli esami.
    - $L: \: s(Studenti)$;
    - $T: \: s.*$;
    - $F: \: \forall p (PianoDiStudi) (\exists e(Esami)(e.MATR=s.MATR \land e.Corso = p.Corso))$.

#### Implicazione logica

- Si introduce l'implicazione $\alpha \implies \beta$.
    - $\alpha \implies \beta$ significa che se $\alpha$ è vero $\beta$ è vero.
    - E se $\alpha$ è falso la formula è comunque vera (non è un'implicazione causale).
    - $\alpha \implies \beta \equiv \lnot \alpha \lor \beta$.
- Elencare gli studenti che hanno superato tutti gli esami del loro indirizzo.
    - $L: \: s(Studenti)$;
    - $T: \: s.*$;
    - $F: \: \forall o(OffertaFormativa)(A \implies B)$.
        - $A = o.Indrizzo = s.Indrizzo$;
        - $B = \exists e(Esami)(e.MATR=s.MATR \land e.Corso=o.Corso \land e.Indirizzo=o.Indirizzo)$.
    - Se si fosse utilizzato $\land$ al posto di $\implies$:
        - Si richiede che tutte le tuple di $OffertaFormativa$ abbiano l'indirizzo $s$.
        - Quindi il risultato sarebbe probabilmente vuoto.
        - L'implicazione invece permette di lavorare su **una parte di tuple**.
    - Analisi:
        - Con la quantificazione universale si scorre tutti i corsi dell'offerta formativa;
        - Con l'antecedente dell'implicazione ci si sofferma solo sui corsi dell'indirizzo dello studente;
        - Con il conseguente dell'implicazione si verifica che lo studente abbia superato l'esame.
        - Se si scorrono corsi che non sono dell'indirizzo dello studente l'antecedente è falso e l'implicazione rimane vera.
            - Il quantificatore universale non viene falsificato (a differenza dell'uso con $\land$).
- Elencare i capi i cui subalterni guadagnano tutti più del capo.
    - Si utilizza la quantificazione universale e l'implicazione.
    - $L: \: i(Impiegati)$;
    - $T: \: i.*$;
    - $F: \: \forall i'(Impiegati)(A \implies B)$.
        - $A = \exists o(Organigramma)(o.Capo = i.MATR \land o.Impiegato = i.MATR)$;
        - $B = i'.Stipendio > i.Stipendio$.
- Trovare i codici degli autori che hanno pubblicato solo in collaborazione.
    - Gli autori che non hanno mai pubblicato una pubblicazione in cui sono gli unici firmatari.
    - Si lavora con $Firme$ perché potrebbero esserci autori che non hanno ancora pubblicato nulla in $Autori$.
    - $L: \: f(Firme)$;
    - $T: \: f.Autore$;
    - $F: \: \forall f'(Firme)(A \implies B)$.
        - $A = f'.Autore = f.Autore$;
        - $B = \exists f'' (Firme) (f''.Pubblicazione = f'.Pubblicazione \land f''.Autore \neq f'.Autore)$.

-----

## Esercitazione

### Esercizio 1

- Elencare nome e cognome dei pazienti che sono stati ricoverati in tutti i reparti.
    - $L: \: p(Pazienti)$;
    - $T: \: p.Nome, p.Cognome$;
    - $F: \: \forall re(Reparti) (\exists ri(Ricoveri)(ri.PAZ = p.COD \land ri.Reparto = re.COD))$.

### Esercizio 2

- Elencare cognome e residenza dei pazienti che sono stati ricoverati in tutti i reparti diretti dal primario '203'.
    - $L: \: p(Pazienti)$;
    - $T: \: p.Cognome, p.Residenza$;
    - $F: \: \forall re(Reparti) (re.Primario = \: '203' \implies \exists ri(Ricoveri)(ri.PAZ = p.COD \land ri.Reparto = re.COD))$.
        - Non bisogna usare un $\land$ in quanto condizione troppo forte, l'implicazione è invece **contestuale**.
        - Usare $\land$ vorrebbe dire che tutti i reparti dell'ospedale sono diretti da quel primario.

### Esercizio 3

- Elencare cognome e nome dei medici che sono primari di un solo reparto.
    - Un medico è primario di un solo reparto se e solo se è primario di almeno un reparto e non è primario di un altro reparto.
    - $L: \: m(Medici)$;
    - $T: \: m.Cognome, m.Nome$;
    - $F: \: \exists re(Reparti)(re.Primario=m.MATR \land A)$;
        - $A = \not \exists re'(Reparti) (re'.Primario = m.MATR \land re'.CODE \neq re.CODE)$.
    - $F: \: \exists re(Reparti)(re.Primario=m.MATR \land B)$;
        - $B = \forall re'(Reparti) (re'.Primario \neq m.MATR \lor re'.CODE = re.CODE)$.
        - Applicazione di De Morgan.
        - Si può modificare utilizzando l'implicazione:
            - $B = \forall re'(Reparti) (re'.Primario = m.MATR \implies re'.CODE = re.CODE)$.

### Esercizio 4

- Elencare nome e nazione dei piloti che non si sono mai piazzati prima dei loro compagni di scuderia nelle gare in una stessa stagione.
    - $L: \: pil(Pilota), pia(Piazzamento)$;
    - $T: \: pil.Nome, pil.Nazione$;
    - $F: pil.Nome = pia.Pilota \land \forall pia'(Piazzamento)(pia'.Anno \land pia'.pilota = pia.Pilota) \implies A$.
        - $A = \not \exists pia''(Piazzamento)(pia''.Nazione = pia'.Nazione \land pia''.Anno = pia'.Anno \: \land$
            - $\land \: pia''.Scuderia = pia.Scuderia \land pia''.Pilota \neq pia'.Pilota \: \land$
            - $\land \: pia''.PosizioneInGara > pia'.PosizioneInGara$.
    - Analisi:
        1. Si parte da una gara di un certo pilota;
        2. Si considera tutte le gare nello stesso anno della gara in $1$ a cui il pilota ha partecipato;
        3. Si controlla che nelle gare al punto $2$ nessun compagno di scuderia si sia mai piazzato dopo.
