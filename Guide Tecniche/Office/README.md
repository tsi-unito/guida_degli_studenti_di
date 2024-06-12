# Una breve guida all'installazione di office

Lo sapevi che UniTO ha una convenzione con Microsoft per il pacchetto di office e OneDrive? Ora sicuramente sì!

Se però procedessi ad installarlo dal sito di Office 365 noteresti che ti vengono installate molti più software di quelli di cui potresti aver bisogno:

- Access
- Skype for Business
- OneDrive
- ...

Ma c'è una soluzione! Per le aziende, Microsoft permette di personalizzare l'installazione, e per fortuna non solo le aziende possono creare una versione personalizzata!

## Requisiti

- 15 minuti
- Saper leggere in Inglese
- Saper scaricare ed eseguire dei file
- Saper interagire con una linea di comando
- Credenziali di Ateneo

Non è necessario abilitare o effettuare alcuna iscrizione, le credenziali di Ateneo sono già abilitate con la licenza di Office.

## 1. Configurazione

Per la configurazione puoi utilizzare quella che ho realizzato, per chi ha bisogno giusto di:

- Word
- Excel
- Power Point
- One Note
- Grammatica Italiana
- Grammatica Inglese

La puoi trovare [qui](Configurazione.xml). Da github in alto a destra scegli "view raw" e poi basta fare click destro sul testo del file e salvare la pagina.

Se per caso desideri cambiare qualcosa o partire da zero, il sito ufficiale del configuratore Microsoft è [questo qui](https://config.office.com/deploymentsettings).

## 2. Installer

Per poter procedere con l'installazione dovremo usare lo _Strumento di distribuzione di Office_, reperibile seguendo i link nella [guida](https://docs.microsoft.com/it-it/deployoffice/overview-office-deployment-tool). Per comodità, [questo è il link diretto](https://www.microsoft.com/en-us/download/confirmation.aspx?id=49117).

Quello che hai scaricato è un software che contiene il vero installer. Per ottenerlo esegui il file (doppio click/seleziona + invio) e scegli una posizione comoda in cui far estrarre lo _Strumento di distribuzione_ (da ora in avanti, _setup.exe_), che effettivamente effettuerà il download e l'installazione di office.

## 3. Installazione

L'installazione è piuttosto banale:

1. Aprire il command prompt
2. Digitare il comando `setup.exe /configure <percorso al file xml di configurazione>`
3. Attendere per la fine dell'installazione, che dovrebbe essere automatica

## Attivazione di Office

Per poter attivare Office, è necessario effettuare l'accesso con le credenziali di Ateneo (niente credenziali di lab).

Buon divertimento!

\- Stefano
