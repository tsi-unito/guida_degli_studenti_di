---
title: "Basi di dati - Basi di dati di esempio"
---

# Basi di dati - Basi di dati di esempio

## Base di dati "Ricoveri"

- Pazienti(**COD**, Cognome, Nome, Residenza, AnnoNascita);
- Medici(**Matr**, Cognome, Nome, Residenza, Reparto);
    - Reparto referenzia Reparti(COD).
- Reparti(**COD**, Nome-Rep, Primario);
    - Primario referenzia Medici(Matr).
- Ricoveri(**PAZ, Inizio**, Fine, Reparto).
    - PAZ referenzia Pazienti(Cod);
    - Reparto referenzia Reparti(COD).

### Tabella Pazienti

| COD  | Cognome  | Nome  | Residenza | AnnoNascita |
| -    | -        | -     | -         | -           |
| A102 | Necchi   | Luca  | TO        | 1950        |
| B372 | Rossingi | Piero | NO        | 1940        |
| B543 | Missoni  | Nadia | TO        | 1960        |
| B444 | Missoni  | Luigi | VC        | 2000        |
| S555 | Rossetti | Gino  | AT        | 2010        |

### Tabella Medici

| MATR | Cognome | Nome   | Residenza | Reparto |
| -    | -       | -      | -         | -       |
| 203  | Neri    | Piero  | AL        | A       |
| 574  | Bisi    | Mario  | MI        | B       |
| 461  | Bargo   | Sergio | TO        | B       |
| 530  | Bell    | Nicola | TO        | C       |
| 405  | Mizzi   | Nicola | AT        | R       |
| 501  | Monti   | Mario  | VC        | A       |

### Tabella Reparti

| COD | Nome-Rep    | Primario |
| -   | -           | -        |
| A   | Chirurgia   | 203      |
| B   | Pediatria   | 574      |
| C   | Medicina    | 530      |
| L   | Lab-Analisi | 530      |
| R   | Radiologia  | 405      |

### Tabella Ricoveri

| PAZ  | Inzio     | Fine      | Reparto |
| -    | -         | -         | -       |
| A102 | 2/05/2014 | 9/05/2014 | A       |
| A102 | 2/12/2004 | 2/01/2005 | A       |
| S555 | 5/10/2014 | 3/12/2014 | B       |
| B444 | 1/12/2004 | 2/01/2005 | B       |
| S555 | 6/09/2015 | 1/11/2015 | A       |

-----

## Base di dati "Impiegati"

- Impiegati(**MATR**, Cognome, Nome, Età, Stipendio);
- Organigramma(**Capo, Impiegato**).
    - Capo referenzia Impiegati(Matr);
    - Impiegato referenzia Impiegati(Matr);

### Tabella Impiegati

| MATR | Cognome | Nome   | Età | Stipendio |
| -    | -       | -      | -   | -         |
| 203  | Neri    | Piero  | 50  | 40        |
| 574  | Bisi    | Mario  | 60  | 60        |
| 461  | Bargio  | Sergio | 30  | 61        |
| 530  | Belli   | Nicola | 40  | 38        |
| 405  | Mizzi   | Nicola | 55  | 60        |
| 501  | Monti   | Mario  | 25  | 35        |

### Tabella Organigramma

| Capo | Impiegato |
| -    | -         |
| 203  | 405       |
| 203  | 501       |
| 574  | 203       |
| 574  | 530       |
| 405  | 461       |

-----

## Base di dati "Esami"

- Studenti(**MATR, NOME**);
- PianoDiStudi(**Corso**);
- Esami(**MATR, Corso**).
    - Corso referenzia PianoDiStudi(Corso);
    - MATR referenzia Studenti(Matr);
- OffertaFormativa(Corso, Indirizzo).

### Tabella Studenti

| MATR | Nome    |
| -    | -       |
| 1    | Rossi   |
| 2    | Verdi   |
| 3    | Bianchi |

### Tabella PianoDiStudi

| Corso          |
| -              |
| Programmazione |
| Basi di dati   |
| Algebra        |

### Tabella OffertaFormativa

| Corso          | Indirizzo |
| -              | -         |
| Programmazione | Sistemi   |
| Basi di dati   | Sistemi   |
| Programmazione | Reti      |
| Basi di dati   | Reti      |
| Algebra        | Sistemi   |

### Tabella Esami

| MATR | Corso          | Indirizzo |
| -    | -              | -         |
| 2    | Programmazione | Sistemi   |
| 3    | Algebra        | Sistemi   |
| 2    | Basi di dati   | Sistemi   |
| 3    | Programmazione | Reti      |
| 2    | Algebra        | Sistemi   |

-----

## Base di dati "Editori"

- Autori(**COD**, Cognome, Nome);
- Pubblicazioni(**ID**, Titolo, Anno);
- Firma(**Autore, Pubblicazione**).
    - Autore referenzia Autori(COD);
    - Pubblicazione referenzia Pubblicazioni(ID).

-----

## Base di dati "GranPremio"

- GranPremio(**Nazione, Anno**, Data, Circuito);
- Piazzamento(**Nazione, Anno, Pilota**, Scuderia, PosizioneInProva, PosizioneInGara, Squalifica, Punti);
- Pilota(**Nome**, Nazione, DataNascita).

-----

## Base di dati "Squadre"

- Circolo(**Nome**, Indirizzo, Città);
- Squadra(**NomeSquadra**, Circolo);
- Giocatore(**TesseraGiocatore**, Nome, Squadra);
- Partita(**NumeroPartita**, Giocatore1, Giocatore2, Vincitore).
    - Si assume che da database $Giocatore1 \neq Giocatore2$.

-----

## Base di dati "Esami"

- Utilizzata per le dipendenze funzionali.
- Esami(**MATR**, NomeS, IndirizzoS, CAPS, CodiceFiscaleS, DataNascitaS, **Corso**, Voto, Lode, DataEsame, CodProf, NomeProf, Qualifica, TipoUfficio).

| MATR | NS      | IS | CAP | CF | DN   | Co    | Vo | Lo | DE     | NP | NP      | Q     | TU     |
| -    | -       | -  | -   | -  | -    | -     | -  | -  | -      | -  | -       | -     | -      |
| 341  | Piero   | TO | 101 | PX | 1990 | BD    | 27 | F  | 1/4/15 | P1 | Anselma | Ric   | ExLab  |
| 343  | Giorgio | NO | 102 | GY | 1991 | Prog1 | 30 | T  | 2/5/15 | B1 | Cardone | ProfA | Piano1 |
| 341  | Piero   | TO | 101 | PX | 1990 | Prog1 | 25 | F  | 3/5/15 | B1 | Cardone | ProfA | Piano1 |
| 343  | Giorgio | NO | 102 | GY | 1991 | BD    | 18 | F  | 6/4/15 | P1 | Anselma | Ric   | ExLab  |

-----

## Base di dati "ContiCorrente"

- Utilizzata per le dipendenze funzionali.
- CC(Titolare, NumeroContoCorrente, NumeroAgenzia, Cittagenzia, DirettoreAgenzia, SaldoContoCorrente, DataUltimaMovimentazione).
