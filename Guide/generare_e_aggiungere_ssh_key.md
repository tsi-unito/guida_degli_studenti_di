# Cos'è una chiave SSH?
Il protocollo SSH (Secure Shell) viene utilizzato per la gestione di reti, sistemi operativi e configurazioni. Le chiavi SSH sono un metodo di autenticazione utilizzato per ottenere l’accesso con connessione crittografata tra sistemi. Una chiave SSH include due chiavi: 
- una chiave pubblica, la quale crittografa i contenuti di nostro interessa. 
- una chiave privata, la quale rimane in possesso solo all'utente. Essa deve essere custodita e non condivisa. 

Nel nostro caso di interesse, ci permette di aprire una linea di comunicazione fra il nostro computer e le nostre repo GitHub/GitLab.

# Come creo una chiave SSH?
- apri il terminale 
- incolla al suo interno questo comando, sostituendo il tuo indirizzo mail (*ed25519* rappresenta l'algoritmo di criptazione)
`ssh-keygen -t ed25519 -C "your_email@example.com"`
- questo comando genera una chiave SSH con la tua mail come etichetta 
`> Generating public/private ALGORITHM key pair.`
- viene richiesto di selezionare un file dove salvare la chiave, premi `INVIO` per accettare quella di default.
- viene richiesto ora di selezionare una password. Se desideri non avere una password, premi `INVIO` due volte
```
> Enter passphrase (empty for no passphrase): [Type a passphrase]
> Enter same passphrase again: [Type passphrase again]
```

# Come aggiungo una chiave SSH al mio account?
Dobbiamo ora trovare la nostra chiave pubblica.
- apri il terminale
  **Linux** 
`cat ~/.ssh/ed25519.pub`
 **Windows** 
`notepad ~/.ssh/ed25519.pub` e copia l'interno contenuto dal notepad
 **MacOS**
`pbcopy < ~/.ssh/ed25519.pub`
- accedi al tuo account 
    - GitLab 
        - premi sulla tua foto profilo, poi *Preferences*
        - premi *SSH Keys*
        - in *Key* incolla il testo
        - in *Title* scegli un nome che ti permetta di identificare facilmente il dispositivo
        - seleziona una data di scadenza. Se non lo desideri, premi sulla x per non farla scadere.
    - su GitHub
        - premi sulla tua foto profilo, poi *Settings*
        - seleziona *SSH and GPG keys*
        - seleziona *New SSH key*
        - in *Title* scegli un nome che ti permetta di identificare facilmente il dispositivo
        - in *Key* semplicemente incolla il testo
        - premi *Add SSH key*

Ora puoi clonare le repo Git tramite protocollo SSH!