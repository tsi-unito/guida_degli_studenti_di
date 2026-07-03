---
title: 'Etica, Società e Privacy - Privacy - Parte V - Differenziazzione'
---

# Etica, Società e Privacy - Privacy - Parte V

## Differenziazzione

### Differential Privacy

>**Differential Privacy** describes a **promise**, made by a data holder, or curator, to a data subject (owner), and the promise is like this: You will not be affected adversely or otherwise, by allowing your data to be used in any study or analysis, no matter what other studies, datasets or information sources are available [Dwork et al, 2006].

- **Differential Privacy** (DP):
    - Una quantità sempre maggiore di dati personali sensibili viene collezionata e archiviata da vari enti.
    - Una sfida tecnologica chiave è quella di progettare sistemi e tecniche di processamento che facciano inferenze su questi dati **mantenendo la privacy** degli individui.
        - Permettendo di **pubblicare i risultati** di queste analisi.
        - Si vuole che un utente possa interrogare più volte un dataset senza che questa permetta una violazione di dati.
    - Si deve trovare un modo per imparare nulla su un individuo imparando però informazioni utili su una popolazione.
    - La DP assicura che qualsiasi sequenza di output sia *essenzialmente* egualmente occorribile. 
        - Indipendentemente dalla presenza o assenza di un qualsiasi individuo.
    - La DP è una **definizione** non un algoritmo.
- Funzionamento del DP:
    - Si assume l'esistenza di un curatore fidato o affidabile che possiede il database $D$.
        - Vari individui (*data owner*) delegano i dati a questo curatore fidato.
    - L'obiettivo è quello di proteggere ogni riga individuale e permettere allo stesso tempo un'analisi statistica generale.
    - Il curatore esegue delle analisi e pubblica queste analisi.
        - Non tutte queste analisi sono sicure in termini di privacy.
        - Alcune potrebbero rivelare che un dato record fosse presente all'interno del dataset originale (segreto).
    - La differential privacy vuole **mascherare** questa informazione.

#### Meccanismo randomizzato

- `def` **Meccanismo randomizzato**: $M(D, q) \to q\widetilde{(D)}$, con $M: \Omega \times Q \to R$.
    - Con il **meccanismo randomizzato** si introduce della **stocasticità** nel processo di calcolo.
    - Una **funzione stocastica** restituisce un qualsiasi valore possibile del codominio $R$.
        - Ogni $r \in R$ ha la sua probabilità di essere estratto.
    - Non si pubblica quindi il risultato della query $q(D)$, ma a questo viene aggiunto del **rumore stocastico**.
        - Il risultato che viene pubblicato viene **estratto da una distribuzione di probabilità**.
        - La query ottenuta, dopo aver inserito rumore, **non è più invertibile**.
    - Il rumore va aggiunto in maniera sensata, per questo vengono introdotti definizioni specifiche di DP.

### $\varepsilon$-differential privacy

- `def` **$\varepsilon$-differential privacy**: un meccanismo (randomizzato) $M: Q \times \Omega \to R$ preserva la $\varepsilon$-DP se $\varepsilon > 0$ e se, per ogni query $q$, ogni coppia di dataset adiacenti $D, D' \in \Omega$ e ogni $r \in R$ si ha $e^{- \varepsilon} \leq \frac{P(M(q, D) = r)}{P(M(q, D') = r)} \leq e^{\varepsilon}$.
    - Notazione:
        - $\Omega$: l'insieme di tutti i dataset.
        - $Q = \{q: \Omega \to R\}$: l'insieme di tutte le query.
        - `def` **Adiacenza**: $D \sim D'$ se $D' = D \cup \{d\}$ o viceversa.
            - Due dataset $D$ e $D'$ se differiscono di un solo record.
            - `!` Si vuole **mascherare la presenza** di questo record (senso della DP).
    - $\varepsilon$ è il **privacy budget** e indica il livello di privacy garantito dal meccanismo.
        - Quando $\varepsilon \to 0$, sia $e^{\varepsilon}$ che $e^{- \varepsilon}$ tendono a $1$.
        - Quando $\varepsilon$ è piccolo si ha che $1 \approx e^{- \varepsilon} \leq \frac{P(M(q, D) = r)}{P(M(q, D') = r)} \leq e^{\varepsilon} \approx 1$.
        - La condizione equivale a chiedere che le due probabilità siano praticamente uguali.
        - Più è **piccolo il privacy budget**, **più è forte la privacy garantita**.
- Privacy e **accuratezza**:
    - Se un meccanismo è ben fatto, il valore più probabile restituito è il valore vero di $q(D)$.
    - Nella DP si richiede che il meccanismo sia privato, non che sia accurato.
    - Possono esistere meccanismi perfettamente privati, ma completamente inaccurati (inutile).
    - È necessario quindi progettare **meccanismi privati e accurati**.

#### Meccanismo di Laplace

- `def` **Meccanismo di Laplace**: $M_{Lap}: (q, D) \to Lap(q(D), GS_1(q)/\varepsilon)$.
    - Costruisce una **distribuzione di Laplace** centrata sul vero valore $q(D)$.
        - Una distribuzione di Laplace è una specie di distribuzione normale più concentrata sul valore centrale.
        - Si tratta di una famiglia di distribuzioni, $Lap(\mu, \sigma)$ che dipende da due parametri:
            - $\mu$: il valore su cui è centrata.
            - $\sigma$: la forma esatta della distribuzione.
                - Più $\sigma$ è piccolo, più la distribuzione è concentrata sul valore centrale.
        - Un meccanismo di Laplace con $\sigma$ piccolo è migliore.
            - In quanto restituirà un valore più vicino a quello centrale, sarà **più accurato**.
            - Questo parametro deve però essere scelto in maniera adatta a **garantire la DP**.
            - $q$ è la funzione che si vuole distorcere, diverse funzioni richiedono diversi livello di distorsione.
        - Quando si vuole garantire molta privacy, le distribuzioni devono essere simili in ogni singolo punto.
            - La differenza deve essere più piccola di $e^{\varepsilon}$ (dalla definizione di DP).
            - Aumentando $\sigma$, le distribuzioni si appiattiscono ma diventano più vicine.
        - $\sigma$ è inversamente proporzionale a $\varepsilon$.
            - Per avere privacy forte, si ha $\varepsilon$ **piccolo** e $\sigma$ grande.
        - Con un dataset piccolo, è necessario aggiungere più rumore.
        - Con due dataset con distribuzioni più distanti, il parametro soglia va rimodulato.
            - Per rispettare la DP, $\sigma$ deve crescere.
            - $\sigma$ **varia al variare** della distanza di $q$.
            - Bisogna trovare la coppia di dataset che fa variare di più $q$.
                - E calibrare la forma della distribuzione basandosi su questa variazione massima.
    - `def` **Meccanismo di Laplace**: $M_{Lap}: (q, D) \to q(D) + Lap(0, GS_1(q)/\varepsilon)$.
        - Si calcola $q(D)$ e gli si somma del **rumore** estratto da una distribuzione di Laplace.
    - `def` **Meccanismo di Laplace**: $M_{Lap}: (q, D) \to q(D) + (X_1, \dots X_n)$ con $q:\Omega to \mathbb{R}^n$.
        - Il meccanismo di Laplace può essere usato anche quando $q$ restituisce un **vettore numerico**.
        - $(X_1, \dots, X_n)$ è il **rumore di Laplace** di ogni componente.
    - `th` Il meccanismo di Laplace preserva la $\varepsilon$-differential privacy.
- `def` **Sensibilità globale**: $GS_1(q) = \max_{D \sim D'} \|q(D) - q(D')\|_1$.
    - La misura della variazione massima di $q$ quando calcolato su due dataset adiacenti.
    - Il meccanismo di Laplace con parametro di scala $\frac{GS_1(q)}{\varepsilon}$ preserva la $\varepsilon$-DP.
        - Ma anche, se il parametro di scala è più piccolo di $\frac{GS_1(q)}{\varepsilon}$ la DP non è più garantita.
        - $\frac{GS_1(q)}{\varepsilon}$ è quindi il parametro ottimale.

##### Limiti del meccanismo di Laplace

- Il meccanismo di Laplace non può sempre essere usata per fornire risposte accurate.
    - **Query non numeriche**: il meccanismo di Laplace aggiunge del rumore numerico al risultato $q(D)$.
        - Non ha senso applicarlo quando $q(D)$ non è un numero.
    - **Selezioni differenzialmente private**:
        - Si immagina che $q$ ritorni, tra tutti i possibili risultati, quello che massimizza una funzione d'utilità $U$.
        - Un meccanismo additivo (come quello di Laplace) potrebbe **distruggere il risultato** $q$.
            - In quanto anche un piccolo rumore aggiungo potrebbe corrispondere a un grande cambiamento nella funzione d'utilità.
        - `eg` Con un acquirente che compra fino a massimo $3.01$, andare a $3.02$ implica non vendere il prodotto.
            - Il cambiamento introdotto dal rumore è minimo ($0.01$) ma la funzione di utilità (guadagno) va a $0$.

#### Meccanismo esponenziale

- `def` **Meccanismo esponenziale**: $p(r) \propto e^{\frac{\varepsilon U(D, r)}{2 GS(U)}}$.
    - $q$ ritorna il valore $r \in R$ che massimizza una funzione d'utilità $U: \Omega \times R \to \mathbb{R}$.
        - $U$ dipende sia da $R$ (l'insieme di tutti i possibili risultati della query $q$) che dal dataset in considerazione.
        - La GS non è più della funzione che si vuole calcolare ma della funzione d'utilità.
    - Il meccanismo assegna a ogni $r \in R$ la probabilità d'estrazione $p(r)$ proporzionale alla sua utilità $U(r)$.
    - L'esatta forma della distribuzione di probabilità dipende da due fattori:
        - $\varepsilon$: il privacy budget;
        - $U$: la funzione d'utilità da massimizzare.
     - $GS(U) = \max_{r \in R} \max_{D \sim D'} |U(D, r) - U(D', r)|$.
        - Si valuta la **variazione massima della funzione utilità**.
    - Funziona anche con le query non numeriche.
    - Fra tutti i risultati possibili, attribuisce una probabilità di estrazione a ognuno, proporzionale a una funzione di utilità.

#### Proprietà della differential privacy

- Si può prendere un algoritmo complesso e suddividerlo in sottofunzioni semplici per cui è possibile calcolare la GS.
    - Per poi ricomporli, ottenendo un algoritmo che possibilmente preserva la DP.
- `th` **Teorema del post-processing**:
    - Sia $M: \Omega \to R$ un meccanismo che preserva la $\varepsilon$-DP.
    - Sia $f$ qualsiasi funzione con dominio $R$.
        - La funzione è relativa solo a $R$, non dipende più dai dati originali.
    - Allora $f \circ M$ preserva la $\varepsilon$-DP.
        - La **trasformazione di risultati DP preserva la DP**.
        - A patto che queste trasformazioni non accedano ai dati segreti originali.
        - Il post-processing non peggiora le garanzie di privacy.
- `th` **Teorema di composizione**: applicazione di più meccanismi allo stesso dataset.
    - Sia $M_1: \Omega \to R$ e $M_2: \Omega \to S$ due meccanismi che preservano rispettivamente $\varepsilon_1$-DP e $\varepsilon_2$-DP ($\varepsilon_1, \varepsilon_2 > 0$).
    - Allora il meccanismo $N: \Omega \to R \times S$ tale che $N(D) = (M_1(D), M_2(D))$ preserva la $(\varepsilon_1 + \varepsilon_2)$-DP.
        - Il meccanismo complessivo che pubblica entrambi i risultati rispetta la DP con livello la somma singoli meccanismi.
        - Il risultato finale è sempre privato, ma lo è **meno rispetto ai singoli meccanismi**.
            - Più informazioni si estrae dal dataset, più è facile individuare un individuo.
        - Non importa come si componga due risultati DP a meno che non si acceda ai dati segreti originali.
- Si può applicare questi due teoremi **senza utilizzare privacy budget**.
    - In quanto si tratta di post-processing, non si lavora sui dataset originali.
- `th` **Teorema di composizione in parallelo**:
    - Sia $D \in \Omega$ un dataset e $M: \Omega \to R$ un meccanismo che preserva la $\varepsilon$-DP.
    - Sia $D_1 \cup D_2$ una partizione di $D$.
    - Allora il meccanismo $N: \Omega \to R \times R$ tale che $N(D) = (M(D_1), M(D_2))$ preserva la $\varepsilon$-DP.
        - Si applica un meccanismo separatamente sui blocchi dei **dati disgiunti**.
        - In fase di pubblicazione, secondo il teorema di composizione, si dovrebbe preservare una $2\varepsilon$-DP.
        - Ma secondo questo teorema, si rispetta la $\varepsilon$-DP, **non c'è peggioramento**.
            - In quanto si applica il meccanismo a sezioni disgiunte del dataset.
            - Se si aggiunge un record, si modifica una parte sola, non entrambe.
            - Ci si occupa di preservare la privacy in solo uno di questi due pezzi non in entrambi.
            - Il privacy budget viene speso solo su uno di questi pezzi.
- Non si ha margine di manovra quando si calcola la forma della Laplaciana.
    - Ma si può decidere **dove applicare il rumore**, come formulare il problema, quale funzione considerare.

### $(\varepsilon, \delta)$-differential privacy

- **$(\varepsilon, \delta)$-differential privacy**: un meccanismo $M:Q \times \Omega \to R$ preserva la $(\varepsilon, \delta)$-DP se, per ogni query $q$, ogni coppia di dataset adiacenti $D, D' \in \Omega$ e ogni risultato $r \in R$, si ha $P(M(q, D) = r) \leq e^{\varepsilon} \cdot P(M(q, D') = r) + \delta$.
    - Sviluppata in quanto spesso la $\varepsilon$-DP potrebbe risultare stringente.
        - Si aggiunge un termine di **tolleranza** $\delta \in (0, 1)$.
        - Si aggiunge meno rumore, ottenendo risultati più precisi.
    - Utile con **dataset grandi**.
    - `th` $M$ preserva la $(\varepsilon, \delta)$-DP se esiste une vento $E$ con $P(E) > 1 - \delta$ tale che $M$ preserva la $\varepsilon$-DP quando $E$ accade.
        - Non importa quando $E$ non si verifica.
    - $\delta$ rappresenta una **probabilità di fallimento** del meccanismo.
        - Si ignorano i casi rari (`eg` estremi) dove la $\varepsilon$ non è preservata.
    - I teoremi di composizione e quello di post-processing sono validi anche in questo caso.
        - Non si sommano solo gli $\varepsilon$, ma anche i $\delta$.
    - Con $\delta = 0$, le due definizioni di DP coincidono, $(\varepsilon, \delta)$-DP $= \varepsilon$-DP.
    - $(\varepsilon, \delta)$-DP con $\delta > 0$ è (molto) **più debole** di $\varepsilon$-DP.
        - Si ammette che il meccanismo può fallire e la privacy **non può essere preservata** in casi speciali.
- La scelta di $\delta$ deve essere quindi guidata da:
    - $\delta$ deve essere il **più piccolo possibile**.
        - In caso di fallimento possono manifestarsi conseguenze molto gravi.
    - La maniera in cui il meccanismo funziona è cruciale nella scelta di $\delta$.
        - Se il meccanismo è molto pericoloso, $\delta$ deve essere prossimo allo zero.
    - Consiglio: scegliere $\delta$ inversamente proporzionale almeno al quadrato del numero di record.

#### Meccanismo della Gaussiana

- `def` **Meccanismo della Gaussiana**: $M_{Gauss}(q, D) \to q(D) + N(0, \sigma^2)$.
    - Aggiunge del **rumore gaussiano** dalla **distribuzione normale** $N$.
        - Alternativo al meccanismo di Laplace, presenta una cuspide molto minore attorno alla media.
        - Aggiunge più rumore rispetto al meccanismo di Laplace.
    - `th` Questo meccanismo preserva la $(\varepsilon, \delta)$-DP dato $\varepsilon, \delta \in (0, 1)$ se $\sigma^2 = \frac{2GS_2(q^2) \log \frac{1.25}{\delta}}{\varepsilon^2}$.
        - Si ha un meccanismo di global sensitivity in norma $2$:
            - $GS_1(q) = \max_{D \sim D'} \|q(D) - q(D')\|_1 = \max_{D \sim D'}(\sum_{k=1}^n |q_k(D) - q_k(D')|)$.
            - $GS_2(q) = \max_{D \sim D'} \|q(D) - q(D')\|_2 = \max_{D \sim D'}\sqrt{\sum_{k=1}^n (q_k(D) - q_k(D'))^2}$.
                - Distanza euclidea.
            - Nel caso delle query scalari norma $1$ e norma $2$ combaciano.
            - Nel caso delle query vettoriali, si **riduce il rumore** inserito.
                - In quanto la norma $2$ potrebbe essere molto più piccola della norma $1$.

##### Differenze tra Laplace e Gauss

- Differenze tra Laplace e Gauss:
    - Meccanismo di Laplace:
        - $\varepsilon$-differential privacy (più forte).
        - Funziona con $\varepsilon>0$.
        - Scala il rumore su L1 GS.
        - Con la stessa GS e $\varepsilon$, aggiunge meno rumore.
        - Laddove è utilizzabile, quello di Laplace è consigliabile.
    - Meccanismo di Gauss:
        - $(\varepsilon, \delta)$-differential privacy.
        - Privacy garantita solo per $\varepsilon < 1$.
        - Scala il rumore meglio su $GS_2$.
            - Meccanismo migliore quando $GS_2(q) \ll GS_1(q)$.
        - La distribuzione normale ha delle buone proprietà di composizione.
            - Sommando due Laplaciane, non si ottiene una Laplaciana.

### Scenari interattivi e scenari non interattivi

- Scenari interattivi e scenari non interattivi:
    - **Scenario interattivo**:
        - Il dataset è mantenuto segreto.
        - Si possono fare molte analisi su un dataset $D$.
            - Ogni analisi consuma parte del privacy budget $\varepsilon$.
            - Con tante analisi, i privacy budget si sommano e si rispetterà poca privacy.
        - Il curatore risponde a un ipotetico analista in maniera $\varepsilon$-DP (dialogo, interattivo).
        - Tipicamente in scenari one-shot.
    - **Scenario non interattivo**:
        - Il dataset è rilasciato privatamente.
            - Per il teorema del post-processing non è necessario aggiungere ulteriore rumore.
            - Bisogna trovare dei metodi per rilasciare privatamente il dataset:
                - Il dataset simile in qualche maniera privata a quelli segreti.
                - Disponibilità di una sinossi dei dati (garantendo $\varepsilon$-DP).
                    - Tante statistiche che descrivono il dataset.
        - Ogni analisi successiva preserva DP.
            - Non c'è consumo di privacy budget (vale il teorema del post-processing).
            - Le funzioni vengono applicate su un dataset.
        - Alcuni ricercatori hanno criticato questo scenario.
            - Bisogna utilizzare degli $\varepsilon$ alti, non proteggendo veramente la privacy.

### Curatore non fidato

- **Curatore non fidato** (untrusted):
    - In tutte le tecniche precedenti si assumeva che il curatore fosse fidato.
        - Si potrebbe non volere che il curatore conoscesse i dati dell'utente.
    - Ogni utente potrebbe **iniettare rumore** sui dati e rivelare solo i dati con rumore. 

#### $(\varepsilon, \delta)$-local-differential privacy

- `def` **$(\varepsilon, \delta)$-local differential privacy**: $P(M(q, x) = r) \leq e^{\varepsilon} \cdot P(M(q, x') = r) + \delta$.
    - Siano $U$ il set di tutti i possibili data records.
        - Si lavora sui data records ($U$) e non sui dataset ($\Omega$).
        - Non si ha $D$ e $D'$ ma $x$ e $x'$.
        - $D$ e $D'$ erano dataset che differiscono di una sola tupla.
    - Aggiunge mediamente più rumore.
    - Può funzionare in uno scenario decentralizzato.
- Differenze tra $(\varepsilon, \delta)$-DP e $(\varepsilon, \delta)$-LDP:
    - La DP si basa su query che prendono in input un dataset ($\Omega \to R$).
        - La LDP si basa su query che prendono in input un singolo record o individuo ($U \to R)$.
    - La DP chiede indistinguibilità per ogni coppia di dataset adiacenti.
        - La LDP chiede indistinguibilità per ogni coppia di record.
    - Tutti i teoremi che valgono per la DP valgono anche per la LDP.
    - Solitamente la DP inserisce più rumore della LDP. 
        - Si rivelano uno per uno i singoli record, è quindi necessario aggiungere più rumore.
        - Non esiste inoltre un concetto di adiacenza, nella LDP si vuole confondere ogni record con ogni altro record.
    - La DP richiede un curatore fidato.
        - La LDP può essere usata quando non è presente un curatore fidato o in uno scenario decentralizzato.
- Esistono dei meccanismi che trasformano una funzione in una **funzione localmente differenzialmente privata**.
    - Il più noto è quello della risposta randomizzata.

#### Risposta randomizzata

- **Risposta randomizzata**:
    - Fornire una risposta sensibile (binaria) può essere determinato tramite un processo randomizzato.
        - `eg` Lanciando una moneta:
            - Con croce si risponde in maniera veritiera;
            - Con testa si rilancia e allora *sì* con testa e *no* con croce.
        - Se la risposta è *sì*, non si può sapere se lo sia perché si è detta la verità o perché uscita testa al secondo lancio.
    - `th` La risposta randomizzata preserva la $\log 3$-local-differential privacy.
        - *Dimostrazione*:
            - $U = \{$tutti i possibili partecipanti dello studio$\}$.
                - $q: U \to \{Yes, \: No\}$.
                - $\forall x \in U$, $M(q, x)$ è l'output di una risposta randomizzata.
            - Sia $x$ uno studio tale che i partecipanti allo studio sia $q(x) = Yes$ (*sì* risposta onesta):
                - Qual'è la probabilità che la risposta randomizzata restituisca *sì*?
                - $P(M(q, x) = Yes) = P(M(q, x) = Yes \mid T) \cdot P(T) + P(M(q, x) = Yes \mid H) \cdot P(H)$.
                    - $= 1 \cdot \frac{1}{2} + \cdot \frac{1}{2} \cdot \frac{1}{2} = \frac{3}{4}$.
                - $P(M(q, x) = No) = P(M(q, x) = No \mid T) \cdot P(T) + P(M(q, x) = No \mid H) \cdot P(H)$.
                    - $= 0 \cdot \frac{1}{2} + \cdot \frac{1}{2} \cdot \frac{1}{2} = \frac{1}{2}$.
                - Il meccanismo funziona bene, la probabilità più alta è quella associata alla risposta corretta.
            - Con $x$ studio tale che i partecipanti allo studio sia $q(x) = No$ (*no* risposta onesta):
                - $P(M(q, x) = Yes) = \frac{1}{4}$ e $P(M(q, x) = No) = \frac{3}{4}$.
            - In entrambi i casi viene sempre privilegiata la risposta reale.
            - Con $x$ e $y$ due partecipanti, se $q(x) = q(y)$ si ha che $P(M(q, x) = r) = P(M(q, y) = r)$.
                - Per ogni $r \in \{Yes, No \}$.
                - Indistinguibilità tra $x$ e $y$ è garantita per ogni $\varepsilon$.
            - Con $x$ e $y$ due partecipanti tali per cui $q(x) = Yes$ e $q(y) = No$ (caso d'interesse della DP):
                - $\frac{P(M(q, x) = Yes)}{P(M(q, y) = Yes)} = \frac{3/4}{1/4} = 3 = e^{\log 3}$.
                - $\frac{P(M(q, x) = No)}{P(M(q, y) = No)} = \frac{1/4}{3/4} = \frac{1}{3} < 3 = e^{\log 3}$.
                - $\frac{P(M(q, y) = No)}{P(M(q, x) = No)} = \frac{3/4}{1/4} = 3 = e^{\log 3}$.
                - $\frac{P(M(q, y) = Yes)}{P(M(q, x) = Yes)} = \frac{1/4}{3/4} = \frac{1}{3} < 3 = e^{\log 3}$.
            - Si è dimostrato che il meccanismo preserva la $\log 3$-local-DP.
- **Risposta randomizzata generalizzata** (su valori non binari con $k$ valori):
    - $P(M(q, x) = v)$:
        - $\frac{e^{\varepsilon}}{e^{\varepsilon} + k - 1}$ se $q(x) = v$.
        - $\frac{1}{e^{\varepsilon} + k - 1}$ altrimenti.
    - Con $\varepsilon > 0$, $e^{\varepsilon} > 1$, quindi il meccanismo ha senso.
        - Quando $\varepsilon$ tende a $0$, $e^{\varepsilon}$ tende a $1$.
            - La probabilità di dare la risposta vera è quasi uguale a quella di dare qualsiasi altra risposta.
            - Ma questo è aspettato, $\varepsilon$ è il PB, più piccolo è più inaccurato è il meccanismo.
        - Quando $\varepsilon$ è grande, $e^{\varepsilon}$ è più grande di $1$, meccanismo accurato.
    - Con $k=2$, si ricade nell'esempio della risposta randomizzata.
        - Quando $k$ cresce, il meccanismo diventa meno accurato.
    - Può essere dimostrato che preserva la $\varepsilon$-DP.
- Si assume che queste procedure vengano applicate a **livello client**.

#### Metodi per preservare la local differential privacy

- Metodi per preservare la local differential privacy
    - Se $q(x)$ ritorna un valore categoriale: generalized randomized response.
    - Se $q(x)$ ritorna un valore numerico: meccanismo di Laplace o Gaussiano.
        - $q(x) + Lap(0, \frac{GS_1(q)}{\varepsilon})$ con $GS_1(q) = \max_{x, x' \in U}\|q(x) - q(x')\|_1$.
            - In questo caso $GS(q)$ è la variazione massima quando $q$ è calcolata su due record differenti $x$ e $x'$.

### Machine Learning e differential privacy

- È necessario stabilire se e come si possa fare **machine learning differenzialmente privato**:
    - Si è iniziato a sviluppare sempre più algoritmi *privati* di ML (o estendendo quelli già noti).
    - L'idea di base è aggiungere rumore nella fase di **training**, così da offuscare il training set.
        - È importante che un modello di ML non sia specifico sui dati d'addestramento (per evitare *overfitting*).
    - ML e DP hanno un goal comune: fornire un risultato accurato non dipendendo troppo dalla presenza di un individuo nei dati.
    - Funzionamento di un processo di ML:
        - Training set $\to$ algoritmo $(A)$ $\to$ modello $(f_{\theta} \to \{C_1, C_2, \dots, C_r\}$).
            - $A: \Omega \to \mathbb{R}^d$ e $D \to \theta$.
        - Tra tutte le funzioni di tipo fisso, quella che descrive meglio i dati viene scelta.
        - Tipicamente, addestrare un modello significa trovare il migliore valore del parametro $\theta$.
        - Si vuole applicare DP sull'algoritmo $A$:
            - **Input perturbation**: aggiungendo rumore sull'input.
            - **In-process mechanism**: aggiungendo durante l'algoritmo.
            - **Output perturbation**: aggiungendo rumore sull'output.

#### DP-k-means

- **DP-k-means**: $C_1, \dots, C_k$ che minimizza $\sum_{i = 1}^k \sum_{X \in C_i} d(X, C_i)$.
    - Algoritmo di clusterizzazione dati.
        - $k$ è il numero di cluster, deciso dall'utente, mediamente da $2$ a $\log(n)$ ($n$ dimensione dataset).
        - La variabile di cluster non è nota neanche nel training set.
    - $d(X, C_I)$ è da considerarsi la funzione di loss, le distanze dei punti di un cluster dal centroide.
    - Il più noto algoritmo per identificare il centroide di un cluster è algoritmo di Lloyd:
        - A ogni iterazione, con i dati iniziali e il centroide calcolato alla precedente iterazione:
            1. Ogni data point è assegnato al cluster corrispondente al più vicino centroide (precedente). 
            2. I nuovi centroidi sono calcolati come la media delle coordinate dei punti in ogni cluster.
        - A ogni iterazione, i centroidi si raffinano fino a convergere.
    - Al passo due dell'algoritmo di Lloyd è applicabile il **meccanismo di Laplace**.
        - Non si calcola la media, ma una media con rumori.
        - Essendo i cluster non sovrapposti, vale il **teorema della composizione parallela**.
        - Si può quindi applicare lo stesso $\varepsilon$ ai vari centroidi, mantenendo (a ogni iterazione) la $\varepsilon$-DP.
    - In generale si ha che vale il teorema di composizione (non quella parallela).
        - Il budget totale vale per il numero $n$ di iterazioni
        - Si ha quindi che si avrà $(n \cdot \varepsilon)$-differential privacy.
            - Ma il numero di iterazioni non è noto a priori, non si può intuire come dividere il budget.

##### DP-k-means con in-process mechanism

- Calcolo della media delle coordinate di un cluster in maniera DP con il metodo Laplace:
    - Si può usare Laplace per calcolare privatamente $q(C) = (|C|, \sum_{x \in C} x_1, \dots, \sum_{x \in C} x_d)$.
        - La query non è scalare, ma vettoriale, si ha la cardinalità e la somma delle componenti.
    - Quando si aggiunge un punto $C$, $|C|$ varia di $1$ (sensibilità $1$).
    - Si assume che tutti i punti appartengono all'iperquadrato $[-1, 1]^d$ (è facile normalizzare).
        - La massima variazione di ogni componente può essere essenzialmente $1$.
    - Si ha quindi che $GS(q) = 1 + d$.
    - Si calcola infine $q(C)$ usando il meccanismo di Laplace con parametro di rumore $\frac{1+d}{\varepsilon}$.
        - E poi calcolare la media di tutte le coordinate.
    - Si tratta di un **in-process mechanism**.
- Come query vengono rilasciati i centroidi.
    - Se si vuole rilasciare l'assegnamento di un punto a un cluster, questo metodo non va bene.

##### DP-k-means con input perturbation

- DP-k-means con input perturbation:
    - Si applica una griglia con $m$ celle al dataset.
    - Si calcola il conteggio dei punti in ogni cella e si sporca il conteggio con $\varepsilon$-DP.
        - Si può applicare lo stesso livello di $\varepsilon$ a ogni cella così da ottenere un dataset $\varepsilon$-DP.
    - Si ha una sensibilità globale $1$.
    - Si ottiene una sinossi (privata) del dataset.
        - Con più celle si ha una sinossi più precisa.
        - Il dataset con conteggi viene fornito all'algoritmo di Lloyd senza che questo implementi altri meccanismi di DP.
    - Non è un meccanismo interattivo.
    - Dimostrato che funzioni meglio di DP-k-means con in-process mechanism.

### Criticità della differential privacy

- **Criticità** della differential privacy:
    - È necessario stabilire come scegliere $\varepsilon$.
    - Decidere cosa fare quando la funzione ha una alta sensibilità globale.
    - Controllare il numero di iterazioni.
        - Tendenzialmente $k$-means converge velocemente.
        - In generale si ha che $n \varepsilon$-DP, 
    - Problemi di convergenza.
        - Sporcando le medie, non è assicurato che si abbia la convergenza.
    - Risultati fuori dominio.
        - In questo caso si potrebbe riapplicare il meccanismo finché non si ha un risultato valido.
        - Oppure proiettare il risultato nel dominio (dimostrato con il teorema del post-processing che preservi la privacy).
    - Impostare i parametri e gli iperparametri quando i dati non sono accessibili.

#### Scegliere $\varepsilon$

- Scegliere $\varepsilon$:
    - Quando $\varepsilon$ è piccolo (vicino a $0$), il significato della definizione è chiaro.
        - Si ha più privacy, ma anche più distorsione.
        - Capita quindi più spesso che si scelga un $\varepsilon$ grande per avere un meccanismo più accurato.
    - `eg` Con $\varepsilon = 8$ (grande) si ha che $\frac{P(M(q, D) = r)}{P(M(q, D') = r)} \leq e^8 \approx 2981$.
        - È permesso che $P(M(D, q) = r) = 1$ e $P(M(D', q) = r) = \frac{1}{2981} = 0.0003$.
        - Ma è evidente che il risultato sia stato calcolato sul primo dataset (che il dataset segreto sia $D$).
        - Nonostante questo meccanismo rispetti la DP, la sua efficacia è bassa.
    - Si sceglie comunque spesso un $\varepsilon$ grande per vari motivi.
        - Si aggiunge più rumore, rendendo impossibile l'inveritibilità da parte dell'attaccante.
        - Il $\varepsilon$ fornito non è quasi mai veritiero.
            - nonostante il teorema di composizione, i vari sottomeccanismi (nella pratica) rispettano più privacy (non provabile).
            - La privacy garantita in realtà è quindi più grande di quella esplicitata da $\varepsilon$.
    - Dwork (2019): avere un registro comune (pubblico) degli $\varepsilon$ utilizzato.
        - Così che gli attacchi su dataset con dato $\varepsilon$ permettano di informare la scelta di $\varepsilon$.
    - Scegliere $\varepsilon$ è quindi un problema aperto.
        - Varia moltissimo dai dati che si usano.
        - Dipende dalla criticità dell'analisi che si vuole fare.
            - Vedere se il rumore aggiunto è accettabile o no (`eg` costo economico).

#### Problemi di sensibilità globale

- Problemi di **sensibilità globale**:
    - Forti outlier possono determinare sensibilità globale infinita.
    - A volte, non è facile derivare analiticamente la GS di una funzione.
    - Possibili soluzioni:
        - Usare la sensibilità locale delle funzioni;
        - Stimare la sensibilità globale delle funzioni dai dati;
        - Esistono metodi che non sono basati sulla sensibilità.
- `def` **Local sensibility**: $LS_1(q, D) = \max_{D' \: t.c. \: D \sim D'} \|q(D) - q(D')\|_1$.
    - Nel calcolo della sensibilità locale non si calcolano tutte i possibili dataset.
    - Ma fissata una tupla, si calcola la sensibilità rispetto a tutti i dataset che differiscono di una tupla.
    - Si ha che $LS_1(q, D) \leq GS_1(q)$, $\forall D \in \Omega$.
        - Si può quindi usare la LS, ma non preserva di default la $\varepsilon$-DP.
        - La LS non può essere dichiarata perché dice qual'è il dataset segreto.
            - Si potrebbe dichiarare un upperbound, un valore (un po') più grande della LS ma che non dipende dal dataset.
            - Allo stesso tempo questo upperbound deve essere più piccola della GS, in modo da diminuire il rumore.
    - Viene introdotto il meccanismo di **propose-test-release**:
        - $GS(q)$ noto a entrambi gli attori, ma $GS(q)$ è troppo alto per l'analista per utilizzarlo nei calcoli.
        - Funzionamento:
            1. L'analista $A$ propone la threshold $b < GS(q)$.
            2. Il curatore calcola la distanza $d$ tra $d$ e il dataset più vicino $D'$ con $LS(q, D') > b$.
                - Anche il test è DP.
            3. Il meccanismo di Laplace è usato per calcolare $\hat{d} = d + Lap(\frac{1}{\varepsilon})$.
            4. Se $\hat{d} > \frac{\ln(1/2\delta)}{\varepsilon}$ il test è passato (rilascio), altrimenti si ritorna un messaggio d'errore.
        - Preserva la $(\varepsilon, \delta)$-DP.

#### Errori tipici nella differential privacy

- Errori tipici nella differential privacy:
    - Calcolare la DP è un task non triviale.
        - Errori nel calcolo della sensibilità.
        - A volte alcuni dati sono acceduti senza iniettare rumore.
        - Cattivo uso dei teoremi di composizione.
        - Differenti definizioni di dataset adiacenti.
    - Esistono dei tool statistici che dato un algoritmo dimostra se questo garantisce la DP.
