---
title: "Basi di dati - Parte III - Architettura dei DBMS"
---

# Basi di dati - Parte III

## Architettura dei DBMS

- Architettura dei DBMS:
    - **Gestore delle interrogazione**: riceve i comandi SQL delle interrogazioni e le trasforma in una forma equivalente ma più efficiente.
        - Ed elabora un piano per la loro esecuzione;
    - **Gestore delle transazioni**: gestisce il ciclo di vita delle transazioni inviando i comandi DML alle componenti sottostanti che devono eseguire l'azione richiesta.
        - Responsabile della proprietà di **consistenza** insieme al serializzatore;
    - **Serializzatore**: fa in modo che l'attuale transazione venga eseguita come fosse l'unica in esecuzione nel sistema.
        - Responsabile della proprietà di **isolamento** (e insieme al gestore delle transazioni di quella di **consistenza**);
    - **Gestore del ripristino**: oltre a gestire il ripristino in seguito a guasti, è responsabile della proprietà di **atomicità** e della **integrità dei dati**;
    - **Gestore del buffer**: responsabile della proprietà di **durabilità**.
