---
title: 'Intelligenza artificiale e laboratorio - Parte III - Esercizi'
---

# Intelligenza artificiale e laboratorio - Parte III - Esercizi

## Valutazione generale

### Esercitazione 1 - Valutazione generale

- Differenze tra un sistema cognitivo artificiale da uno AI standard?
    - Potere esplicativo (principale)
    - Gli ambienti complessi non sono dirimenti.
- Distinzione tra sistemi con design funzionalista strutturalista è applicabile a **tutti i tipi di sistemi**.
- Tipi di spiegazione più utili per spiegare un sistema cognitivo artificiale:
    - Spiegazione causale;
    - Spiegazione meccanicistica;
    - Spiegazione evoluzionistica;
    - Spiegazione teleologica;
    - Spiegazione funzionalista.
- Stabilire il grado di plausibilità biologica o cognitiva di un sistema artificiale
    - Minimal Cognitive Grid
- Secondo la prospettiva enactive un LLM può essere definito intelligente?
    - No, perché questi sistemi sono disincarnati (non embodied).
    - Un sistema non incarnato non può definirsi per definizione intelligente.
    - Lo sviluppo di questa competenza linguistica deve essere legata allo sviluppo di altre competenze motorie.
- Livello irrilevante della gerarchia di Marr irrilevante nel PSSH?
    - Non sono tutti rilevanti, il livello di implementazione/fisico è irrilevante.
    - Agli antipodi rispetto alla prospettiva enactive.
- Quando si applica il concetto di intentional stance? Spiegazione teleologica.
- Test di Turing:
    - Può essere usato come strumento per valutare i progressi dei sistemi artificiali nell'elaborare del linguaggio.
    - Non può essere usato come test generale per valutare l'intelligenza di sistemi artificiali.
        - Nella storia è stato interpretato in questa maniera, ma si tratta di un **test esclusivamente linguistico**.
- Avere un sistema artificiale in grado di ottenere lo stesso comportamento di uno naturale in un task intelligente ci permette di attribuire a questo sistema le stesse capacità intellettive assunte per il sistema naturale?
    - No, un'analisi delle performance e degli errori non è sufficiente.

### Esercitazione 2 - Valutazione generale

- La procedura di subgoaling in SOAR consiste:
    - Nel superamento di uno stato d'impasse `<s>` tramite la creazione di un sottostato `<s1>` in memoria di lavoro.
- L'architettura di sussunzione assume:
    - Una interazione costante con l'ambiente usato come modello del mondo.
    - Non è presente la conoscenza di senso comune.
- Quali sono i limiti dell'architettura di sussunzione di Rodney Brooks?
    - L'interazione con l'ambiente fa imparare le regole d'apprendimento.
    - Nell'ambito dei modelli emergentisti non supervisionati, l'unico meccanismo d'apprendimento biologicamente giustificato è l'**apprendimento hebbiano**.
    - Difficoltà a raggiungere abilità cognitive d'alto livello che non afferiscano alla sola percezione.
- Quali sono gli elementi di ispirazione cognitiva su cui si basa SOAR?
    - Distinzione tra memoria a breve e lungo termine.
    - Il mapping del ciclo decisionale di SOAR e la Newell's Timescale of Human Action.
    - Subgoaling (means-end analysis, dalla psicologia cognitiva).
    - Memoria delle preferenze.
    - Il Reinforcement learning è invece di ispirazione behavioristica.
- Perché un sistema simbolico (pur non essendo biologicamente plausibile) può giocare un ruolo esplicativo circa la comprensione di meccanismi strutturali della cognizione?
    - Il paradigma specifica il livello di astrazione con cui si valuta un task.
    - Può esserlo, deve però essere sviluppato con un **approccio strutturalista** (quello biologicamente ispirato non è l'unico).
        - Per stabilire il ruolo esplicativo bisogna **valure l'approccio e non solo il paradigma**.
    - Esistono esempi di modelli computazionali che hanno aiutato a conoscere meccanismi cognitivi (criptoaritmetica).
- Quali sono i limiti di una architettura come SOAR?
    - Ha i limiti di un approccio fortemente simbolico.
    - La base di SOAR è la PSSH, si ignora la parte biologica.
- Quali sono gli elementi architetturali in comune alle principali architetture che formano lo Standard Model of Mind?
    - Diversi tipi di memoria (breve e lungo termine), modulo di percezione e modulo di azione.
- Quali sono i limiti dello Standard Model of Mind?
    - Si tratta di un modello molto (troppo) astratto, senza specifica, **non generale ma generico**.
        - Non descrive, per esempio, come avviene la comunicazione tra i moduli.
        - Non è un'architettura infatti, è un modello.
        - La quasi totalità delle attuali architettura sono una istanziazzione di questo modello.
        - Sottolineare i limiti di un modello, non quelli di un'architettura (gestione incertezza, ecc).

-----

## Spiegazione

### Esempio 1 - Tipi di spiegazione

- Cambiamento del colore dei camaleonti:
    - I camaleonti cambiano colore in risposta a condizioni fisiologiche e stimoli di vario tipo.
    - Explanandum:
        1. Perché cambiano colore?
        2. Perché assumono variazioni di diverso colore in base al predatore?
- Explanans teleologici:
    1. Per mimetizzarsi e sfuggire dal predatore; 
    2. Per sfuggire agli uccelli che hanno vista migliore e maggiore capacità di discriminazione dei colori.
- Explanans meccanicistica:
    1. Il cambiamento di colore dei camaleonti è dovuto alla risposta di alcune cellule del derma dell'animale (i cromatofori) a stimoli nervosi e/o endocrini.
- Explanans evoluzionistica:
    2. Il numero di predatori volatili nell'habitat dei camaleonti è maggiore rispetto a quello di altri animali e ciò impone una maggiore pressione selettiva rispetto alla capacità mimetica

-----

## Architetture cognitive

### Esempio 1 - SOAR

- Un robot può solo mangiare e bere.
    - Inizialmente è affamato e assetato.
    - Goal: fare in modo che non lo sia più.

```
(<s> ^hungry yes ^thirsty yes)

sp { ht*propose-space*ht 
    (state <s> -^impasse ^susperstate nil)
    -->
    (<s> ^name st-state)
    (<a> ^problem-space <s> ^desired <d>)
    (<p> ^name hungry-thristy)
    (<d> hungry no)
    (<s> ^thirtsty yes ^hungry yes)
}

sp { ht*init-state*ht 
    (state <s> -^impasse ^susperstate nil)
    -->
    (<s> ^name st-state)
    (<p> ^name hungry-thristy)
    (<d> hungry no)
    (<s> ^thirtsty yes ^hungry yes)
}

sp { ht*propose-op*eat
    (state <s> ^problem-space.name hungry-thirsty)
    (<s> ^name st-state)
    -->
    (<s> ^operator <o>)
    (<o> ^name eat)
}

sp { ht*apply-op*eat
    (state <s> ^operator <o>)
    (<o> ^name eat)
    (<s> ^hungry yes)
    -->
    (write (clrf) |     chomp chomp...  |)
    (<s> ^hungry yes - no +)
}

sp { ht*terminate*eat
    (state <s> ^operator <o>)
    (<o> ^name eat)
    (<s> ^hungry no)
    -->
    (<s> ^operator <o> @)
}

sp { ht*propose-op*eat
    (state <s> ^problem-space.name hungry-thirsty)
    (<s> ^name st-state)
    -->
    (<s> ^operator <o>)
    (<o> ^name drink)
}

sp { ht*apply-op*drink
    (state <s> ^operator <o>)
    (<o> ^name drink)
    (<s> ^thirsty yes)
    -->
    (write (clrf) |     glup glup...  |)
    (<s> ^thirsty yes - no +)
}

sp { ht*terminate*drink
    (state <s> ^operator <o>)
    (<o> ^name drink)
    (<s> ^thristy no)
    -->
    (<s> ^operator <o> @)
}

# Si assegna una preferenza al mangiare che al bere
sp { ht*compare*eat*better*drink
    (state <s> ^desired <d> ^problem.space.name hungry-thirsty)
    (<s> ^hungry yes)
    (<d> ^hungry no)
    (<s> ^operator <op-eat> +)
    (<op-eat> ^name eat)
    (<s> ^operator <op-drink> +)
    (<op-drink> ^name drink)
    -->
    (<s> ^operator <op-eat> > <op-drink>) # Con entrambi gli operatori applicabili, > fa prediligere la prima regola
}

sp { ht*evaluate*state*success
    (state <s> ^desidered <d>)
    (<d> ^hungry <val>)
    (<s> ^hungry <val>)
    -->
    (<s> ^success <d>)
}
```
