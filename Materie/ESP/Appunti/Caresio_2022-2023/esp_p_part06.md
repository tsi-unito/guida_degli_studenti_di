---
title: 'Etica, Società e Privacy - Privacy - Parte VI - Privacy in contesti distribuiti'
---

# Etica, Società e Privacy - Privacy - Parte VI

## Privacy in contesti distribuiti

### Architettura standard della data analysis

- **Architettura standard della data analysis**:
    - Molti dati locali (repository su server locali);
    - Raccolte in una warehouse;
    - Su quest'ultima vengono effettuate **analisi** (data mining, ML).
        - I singoli DB potrebbero non aver abbastanza valenza statistica e di generalizzazione.
    - Questo modello non garantisce la privacy.
        - Il curatore della data warehouse ha accesso a tutti i dati.
        - Si possono volere diversi tipi di confidenzialità per singole fonti.
        - Le analisi spesso vengono assegnate a aziende terze.
- Limitazioni dell'architettura standard che hanno portato alla **distributed data analysis**:
    - Performance;
    - Connettività;
    - Eterogeneità delle fonti;
    - Privacy delle fonti.
- Fare un'analisi sulle varie fonti locali non migliora la situazione.
    - L'accentramento della warehouse viene spostata alla fase di data analysis.

### Private distributed mining

- **Private distributed mining**:
    - Presenta un modulo di data analisi locale per ogni fonte.
    - Le analisi locali vengono combinate da un modulo *data analysis combiner*.
    - Si tratta di un modello molto generico.
- Privacy-preserving data analysis:
    - Chi: 
        - Governi / agenzie pubbliche, collaborazioni industriali / trade groups, multinazionali, uso pubblico di dati privati.
        - Le aziende tendenzialmente rendono pubblici solo i fornitori poco affidabili.
    - Classi di soluzioni:
        - *Data obfuscation*: nessuno vede i dati reali.
        - *Summarization*: solo i fatti necessari sono esposti.
        - *Data separation*: i dati rimangono alle parti fidate.

#### Data separation

- **Data separation**:
    - Obiettivo: solo le parti fidate vedono i dati.
    - Approcci:
        - I dati sono tenuti dal posseditore/curatore.
        - Rilascio limitato a terze parti fidate.
        - Operazioni e analisi eseguite da parti fidate.
    - Problemi:
        - Non è detto che le parti fidate vogliano fare le analisi.
        - Le analisi risultanti potrebbero rilasciare informazioni private.
    - I data holders non devono condividere i propri contenuti.
        - In quando non previene modelli globali.

##### Secure Multiparty computation

- **Secure Multiparty computation**:
    - Obiettivo: calcola una funzione sui dati dove ogni parte ha solo una parte degli input.
    - *Problema del milionario di Yao*:
        - Due milionari, Alice e Bob, sono interessati a sapere chi è il più ricco (senza rivelare).
        - Il calcolo sicuro è possibile se la funzione può essere rappresentata come un circuito.
            - Valida per circuiti molto semplici (funzioni booleane).
    - Funziona per parti multiple (con più di due parti).
        - Spesso l'overhead però è tale da non renderlo praticabile.
    - Funzionamento:
        - Ogni lato ha input, conosce i circuiti per calcolare la funzione.
        - Aggiunge un valore casuale al suo input, e dà l'altra parte all'altro lato.
            - Ogni lato deve *condividere* tutti gli input.
        - Si calcola parte dell'output.
            - Si sommano i risultati alla fine.
        - Porte utilizzabili:
            - XOR gate: semplice calcolo locale.
            - AND gate: si invia la parte codificata in una tabella di verità.
                - L'oblivious transfer permette alle altre parti di ottenere solo valori corretti dalla tabella.

##### Oblivious transfer

- **Oblivious transfer**:
    - Asimmetrico.
        - $A$ ha degli input $a_j$.
        - $B$ fa le scelte.
        - $A$ non sa le scelte, $B$ vede solo il valore scelto.
    - Funzionamento:
        - $A$ invia la chiave pubblica $p$ a $B$.
        - $B$ seleziona $4$ valori casuali da $b$.
            - Di questi quattro, cifra (solo) $b_{choice}$ con $f_p$, invia tutto a $A$.
        - $A$ decifra tutto con la chiave privata, invia a $B$: $c_i = a_i \bigoplus e(f_p^{-1}(b_i))$.
            - $A$ ottiene da $B$ dei valori cifrati, li decifra (sia quelli non scelti che quello scelto).
            - Calcola quattro valori $c$ come $a_i$ (input che possiede) in XOR con la decodifica di $b_i$ effettuata con la chiave privata.
            - Lo fa per tutti i valori di $b$, si avranno quindi $4$ valori, ma $A$ non sa quale sia il valore reale.
        - $B$ restituisce $c_{choice} \bigoplus e(b_{choice}) = a_{choice} \bigoplus e(f_p^{-1}(f_p(b_{choice}))) \bigoplus e(b_{choice})$.
            - $B$ non sa quale siano gli $a_i$ ma sa quale dei $c_i$ sia corretto. 
            - Decodifica quindi $c$ scelto (quello corretto) in XOR con $b$ scelto.
    - Il risultato ottenuto è lo stesso che si sarebbe ottenuto calcolando la **funzione in maniera chiara**.
        - Il vantaggio è che un nodo ha gli input e l'altro fa le scelte.
    - Se si fanno fare le stesse azioni in **maniera simmetrica**, si ottiene un **protocollo completo di SMC**.
        - Questo può essere utilizzato per calcolare dei DT privacy-preserving.

##### Decision Tree Construction

- **Decision Tree Construction**:
    - Funziona solo su protocolli con due parti.
        - Con più parti diventa veramente oneroso.
    - Two-party horizontal partitionng:
        - Ogni sito ha lo stesso schema.
        - Il set di attributi è noto.
        - Le tuple vengono partizionate.
        - Mantenere la privacy delle singole tuple.
    - Apprende un classificatore decision tree.
        - Utilizza ID3 per creare l'albero.
    - Si adotta essenzialmente ID3 in combinazioni con definizioni di SMC.
        - Queste vengono applicate a più livelli.
    - Limitazioni di sicurezza:
        - Il protocollo viene adottato in un modello semi-onesto.
            - È possibile memorizzare le computazioni intermedie.
            - Con attacchi di bruteforce è possibile ricostruire i valori reali.
        - Solo i casi con due parti sono considerati.
        - Calcola un'approssimazione ID3.
            - Si cerca un'approssimazione $ID3_{\delta}$ tra tutte le $ID_3{\delta}$ possibili.
            - $\delta$ è il parametro di approssimazione di ID3, e ha implicazioni di efficienza.
        - Funziona solo su attributi categorici.
- Funzionamento dell'algoritmo ID3 (non privato):
    1. Calcola l'entropia/information gain (IG) di ogni feature.
    2. Se tutte le righe non appartengono alla stessa classe, splitta il dataset in subset.
        - Con le feature con entropia minima (o IG massimo).
    3. Crea un nodo del DT usando la feature con entropia minima o massimo IG.
    4. Se tutte le righe appartengono alla stessa classe, rendi il nodo attuale una foglia con classe sua etichetta.
    5. Ripeti per le rimanenti feature finché non sono esaurite.
        - O finché il DT ha solo nodi foglie.

##### Decision Tree Construction (privato)

- Funzionamento dell'algoritmo ID3 (**privato**):
    - Dati: $A$ set degli attributi, $C$ classe dell'attributo, $D$ set di istanze di dati.
    - Step:
        1. Calcola l'entropia/information gain di ogni attributo nel data set $D$.
        2. Divide il set $D$ in subset usando l'attributo per cui la risultante entropia (post-splitting) è minima (o IG massima).
            - Si ottengono classi più pulite, dove le classi sono distribuite in maniera pulite nelle partizioni.
            - In ogni gruppo si avrà una prevalenza di una certa classe rispetto a un'altra.
        3. Crea un nodo del DT contente l'attributo.
        4. Esegui ricorsivamente sui subset usando gli attributi rimanenti.
    - Step $1$: se il set $A$ è vuoto, si ritorna un nodo-foglia con la classe assegnata alla maggior parte delle transazioni in $D$.
        - Arrivati al nodo foglia, si può fare una predizione.
            - Si associa una variabile di classe a un nodo fogli.a
        - Il set di attributi è pubblico, entrambi sanno se $A$ è vuoto.
        - Si applica il protocollo di Yao.
            - Input: $(|D_1(c_1)|, \dots, |D_1(c_L)|), (|D_2(c_1)|, \dots, |D_2(c_L)|)$.
            - Output: $i$ dove $|D_1(c_i) + |D_2(c_i)|$ è il più grande.
                - La variabile di classe associata, la somma delle due cardinalità si può fare in maniera privacy-preserving utilizzando la **SMC**.
            - Prima parte in cui si applica la SMC.
    - Step $2$: se $D$ consiste di transazioni che hanno tutti lo stesso valore $c$ per lo stesso attributo classe, restituisci un nodo foglia con valore $c$.
        - Si rappresenta il nodo foglia che ha più di una classe (nel transaction set) con un simbolo fissato differente da $c_i$.
            - Si forzano le parti a inserire in input questo simbolo fissato o $c_i$ (valore della classe reale).
            - Check equality to decide if at leaf node for class $c_i$.
            - Si fa un check dell'uguaglianza per decidere se nel nodo foglia prevale effettivamente $c_i$.
                - Vari approcci per l'*equality checking* basati sulla **SMC**.
    - Step $3$:
        - a. Determinare l'attributo che meglio classifica le transazioni in $D$ (chiamato $A_i$).
            - Ovvero l'attributo da utilizzare per lo split.
            - Essenzialmente si calcola in maniera sicura (**SMC**) l'entropia $x * (\ln x)$.
                - Non esiste un circuito semplice per calcolarlo.
            - Si utilizza il protocollo di Yao per calcolare un'approssimazione (molto rozza) di $\ln x$.
                - Si utilizzano le serie di Taylor per calcolare $ln(1 + \varepsilon)$.
                    - Gli addendi della serie possono essere calcolati con metodi di **SMC**.
                - Più l'approssimazione è precisa, maggiore è l'overhead dell'algoritmo.
        - Si chiama ricorsivamente $ID3_{\delta}$ per i restanti attributi nel transaction set $D(a_1), \dots, D(a_m)$.
            - Dove $a_1, \dots, a_m$ sono i valori dell'attributo $A_i$ (trovato allo step $a$).
        - Dato che i risultati di $3(a)$ e gli attributi valori sono pubblici, entrambe le parti possono partizionare individualmente il DB e preparare i loro input per le chiamate ricorsive.
    - Si hanno varie fase di calcolo precise e alcune approssimate.

#### Altri approcci complementari

- Functional encryption:
    - Generalizzazione della cifratura a chiave pubblica.
    - In cui possedere una chiave segreta permette di apprendere una funzione senza rivelarne gli input.
    - Ogni parte accede al valore della funzione senza accedere ai dati.
- (Fully) homomorphic encryption:
    - Permette di calcolare funzioni su dati cifrati senza decifrarli prima.
        - Le operazioni possono essere applicate da chiunque abbia la chiave pubblica.
    - Il calcolo risultante sono lasciate in forma cifrata.
    - Il risultato è lo stesso come se si fosse lavorato sui dati cifrati.
- Federated learning:
    - Permette di addestrare un algoritmo su più device decentralizzati con dati locali, senza scambiarli.

### Federated learning

- **Federated learning**:
    - Accedere a dati privati sta diventando sempre più complicato.
        - User awareness;
        - Centralizzazione dei dati impossibilitata da vincoli legati.
        - Legislazione (GDPR, ecc).
    - Ma i modelli di ML, soprattutto di DL, richiedono una quantità enorme di dati.
    - Servono tecniche di apprendimento privacy-preserving e **federato**.
- **Federate learning**:
    - Contesto di ML dove **più entità** (client) **collaborano** a risolvere un problema di ML.
    - Usando spesso la **coordinazione di un server centrale** (**aggregator**).
    - I dati di ogni client sono **memorizzati localmente e non sono trasferiti**.
    - Degli aggiornamenti per l'immediata aggregazione sono usate per raggiungere l'obiettivo di apprendimento.
    - Modelli:
        - Cross-device FL: milioni di client, relativamente piccoli dataset locali.
        - Cross-silo FL: numero piccolo di client ($<100$, $<10$), dataset grandi e connessioni stabili.
        - Horizontal FL: ogni client possiede un insieme un subset del training set.
        - Vertical FL: ogni client possiede un sottoinsieme di feature di (potenzialmente) tutti gli esempi.
    - Assunzioni:
        - I parametri del modello **non contengono** più informazione che il training data.
        - La dimensione del modello è generalmente più piccola che la dimensione del training set.
    - Obiettivi:
        - **Confidenzialità**: i client non condividono i dati.
        - Utilità: i client devono ricevere benefici dal partecipare alla federazione.
    - Desiderata:
        - Il modello federato deve essere vicino al *modello ideale*.
    - Sfide maggiori:
        - Non-IID: i dati generati dagli utenti possono essere molto divesi.
        - Unbalanced: alcuni utenti producono molti più dati di altri (maggior peso).
        - Massively distributed: numero di client enorme.
        - Comunicazione limitata: setting non stabili, spesso solo un subset degli utenti partecipa all'iterazione.

#### Protocollo generale di Federated Learning

- **Protocollo generale di Federated Learning**:
    1. L'aggregatore inizializza il modello globale e lo condivide con i client.
    2. I client aggiornano il modello (training locale) con i loro dati.
    3. I client mandano al server il modello (locale) aggiornato.
    4. Il server **aggiorna il modello globale** aggregando quelli ricevuti. 
    5. Il server manda il modello globale ai client, ripetendo il procedimento.

#### Federated Stochastic Gradient Descent

- **Federated Stochastic Gradient Descent** (FedSGD):
    - Selezionare un client con $n_k < n$ è come scegliere un batch nel classico SGD.
    - Un singolo step di gradient descent è fatto per round.
    - Nel FL cross-device si ha solo una $C$-fraction dei client seleziona a ogni round:
        - Ci sono molti criteri di selezione: dispositivi in carica, in idle, ecc.
        - $C=1$: full-batch (non-stochastic) gradient descent (non realistico).
        - $C<1$: stochastic gradient descent (SGD).
    - Funzionamento:
        - Si assume che l'aggregatore inizializzi il modello globale $w$.
        - In un round $t < T$:
            - L'aggregatore condivide l'attuale modello globale $w$ a ogni client.
            - Ogni client $k$ calcola l gradiente sui dati locali (tipicamente un singolo batch).
        - Alternativa $1$:
            - Ogni client $k$ invia $g_k$ (gradiente locale).
            - L'aggregatore aggrega (media pesata) i gradienti per generare un nuovo modello globale.
        - Alternativa $2$:
            - Ogni client calcola il gradiente sul proprio modello locale.
            - Il server centrale performa le aggregazioni dei modelli.
        - Essendo il calcolo del gradiente un'operazione lineare, le due alternative sono equivalenti.
    - La comunicazione di FedSGD è altamente inefficiente.
        - Un client che partecipa a un round invia e riceve un modello a ogni (mini-batch) update.
    - Si migliora l'efficienza dei calcoli:
        - Si selezionano più client a ogni round, così da avere una stima del gradiente più affidabile.
        - **Aumentare la computazione su ogni client**.

#### Federated Average

- **Federated Average** (FedAVG):
    - Funzionamento:
        - Si assume che l'aggregatore inizializzi il modello globale $w$.
        - In un round $t < T$:
            - L'aggregatore condivide l'attuale modello globale $w$ a ogni client.
            - Ogni client $k$ calcola il gradiente sui suoi dati locali.
            - Ogni client $k$ calcola **per $E$ epoche** il gradiente (come nel caso 2 del FedSGD).
                - Calcola local mini-batch SGD.
    - Se $E = 1$ e il batch size è $n_k$ FedSDG $=$ FedAvg.
    - FedAvg funziona decentemente nella pratica ed è l'approccio più utilizzato.
    - FedAvg non garantisce la convergenza lineare per *loss* lisce, fortemente convesse.
        - Con molte epoche locale, i modelli dei vari client potrebbero andare in posizioni diverse.

#### Federated Learning decentralizzato

- **Federated Learning decentralizzato**:
    - Si abbandona l'utilizzo di un aggregatore centrale.
    - Funzionamento:
        - Ogni client inizializza il modello in maniera casuale.
        - Dopo del tempo, un client manda un modello a un altro client.
        - I client sono quasi sempre in ascolto, ricevono dei modelli da altri.
            - Il modello può essere accettato o meno, e poi viene aggregato localmente.

#### Limiti di privacy del Federated Learning

- **Limiti di privacy del Federated Learning**:
    - La privacy nel FL si basa sul fatto che i dati privati rimangano in locale sui device federati.
        - Ma il modello potrebbe far trapelare informazioni del training set.
        - Per reti neurali con specifiche configurazioni, l'input della rete può essere ricostruito unicamente dai gradienti della rete (teorema).
    - FL aiuta la privatezza ma **non la garantisce**.
    - Tipi di attaccanti:
        - Semi-onesti (o onesti-ma-curiosi):
            - Cercano di imparare stati privati degli altri partecipanti senza deviare dal protocollo del FL.
        - Malicious:
            - Cercano di imparare stati privati degli altri partecipanti deviando dal protocollo del FL.
            - Modificando, riapplicando o rimuovendo messaggi.
    - Tipi di attacchi:
        - Membership inference: capire quali dati sono stati utilizzati per fare traning.
        - Model inversion: si inferiscono i dati (non sono noti, a differenza del precedente).
- Soluzioni relative agli attacchi alla privacy del FL:
    - Statistici: differential privacy.
    - Crittografici: homomorphic encryption e secure multiparty computation.
        - Secure Aggregation: classe di algoritmi di SMC dove un gruppo di parti non fidate mutualmente collaborano a calcolare un valore aggregato senza rivelare ad altri informazioni private tranne quelle imparabili dall'aggregato.
            - Si possono creare specifiche **maschere** che le parti aggiungono o rimuovo dal proprio modello.
            - In fase di aggregazione, le maschere si elidono a loro volta.
    - Hardware: trusted execution environment.
- Le soluzioni privacy-preserving hanno serie criticità (computazionali e/o a livello di performance).
