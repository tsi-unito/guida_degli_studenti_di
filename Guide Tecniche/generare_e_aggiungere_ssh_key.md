# Cos'è una chiave SSH?
Il protocollo SSH (Secure Shell) viene utilizzato per la gestione di reti, sistemi operativi e configurazioni. Le chiavi SSH sono un metodo di autenticazione utilizzato per ottenere l’accesso con connessione crittografata tra sistemi.

Una chiave SSH include due chiavi: 
- Una pubblica, la quale crittografa i contenuti di nostro interesse;
- Una privata, la quale rimane in possesso solo all'utente. Essa deve essere custodita e non condivisa.

Nel nostro caso di interesse, ci permette di aprire una linea di comunicazione fra il nostro computer e i nostri repository Git.

## Requisiti

- Git installato sul sistema operativo
- 

## Come creo una chiave SSH?
1. Apri il terminale 
2. Incolla al suo interno questo comando, sostituendo il tuo indirizzo mail (*ed25519* rappresenta l'algoritmo di criptazione)  
`ssh-keygen -t ed25519 -C "your_email@example.com"`
3. Questo comando genera una chiave SSH con la tua mail come etichetta.
    ```
    > Generating public/private ALGORITHM key pair
    > Enter passphrase (empty for no passphrase): [Type a passphrase]
    > Enter same passphrase again: [Type passphrase again]
    ```
4. Viene richiesto di selezionare un file dove salvare la chiave, premi `INVIO` per accettare quella di default. ⚠ Attenzione: se una chiave esiste già, verrà sovrascritta da quella che si sta creando in questo momento.
5. Viene richiesto ora di selezionare una password. Se desideri non avere una password, premi `INVIO` due volte. Questo può risultare conveniente per non doverla inserire frequentemente, ma non è suggerito in ambiti in cui la sicurezza deve essere posta prima di tutto.

## Come aggiungo una chiave SSH al mio account?
Dobbiamo ora trovare la nostra chiave pubblica.

⚠⚠ PER NESSUNA RAGIONE DEVE ESSERE CONDIVISA LA CHIAVE PRIVATA!! ⚠⚠
1. Apri il terminale  
    - **Linux**     
	`cat ~/.ssh/id_ed25519.pub`  
    - **Windows**  
  Vai nella cartella del tuo utente (generalmente con Windows 10 è sotto `C:\Users\<Nome Utente>`) e apri la cartella `.ssh` (è una cartella nascosta, assicurati di averne abilitato la visualizzazione dalle impostazioni dell'esplora risorse).  
  Apri il file `id_ed25519.pub` con il tuo text editor di fiducia e copia l'intero contenuto: `CTRL+A` -> `CTRL+C`.  
    - **MacOS**  
	`pbcopy < ~/.ssh/id_ed25519.pub`  
2. Accedi al tuo account 
    - **GitLab**  
        3. Premi sulla tua foto profilo, poi *Preferences*  
        4. Premi *SSH Keys*  
        5. In *Key* incolla il testo  
        6. In *Title* scegli un nome che ti permetta di identificare facilmente il dispositivo  
        7. Seleziona una data di scadenza. Se non lo desideri, premi sulla x per non farla scadere.
    - **GitHub**  
        3. Premi sulla tua foto profilo, poi *Settings*  
        4. Seleziona *SSH and GPG keys*  
        5. Seleziona *New SSH key*  
        6. In *Title* scegli un nome che ti permetta di identificare facilmente il dispositivo  
        7. In *Key* semplicemente incolla il testo  
        8. Premi *Add SSH key*

Ora puoi clonare le repo Git tramite protocollo SSH!
