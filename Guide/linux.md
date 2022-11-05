# La guida all-in-one al mondo GNU-Linux @ UniTO (WIP!)

## Preambolo

### Come mai questa guida?

Cope. _(questa guida è frutto di coping)_

### Linux o GNU-Linux?

Linux è il kernel, GNU-Linux è il layer aggiunto che lo eleva a sistema operativo.
Ma non è tutto: GNU-Linux non è in se usabile dal normale utente, se non messo nel contesto di una _distribuzione_.
La distribuzione è il vero e proprio sistema operativo che funziona. Non andrò nei dettagli, il web vi aiuterà di più su [questo problema terminologico](https://stallman-copypasta.github.io/).
Quando in questa guida parlerò di Linux, intenderò la distribuzione utilizzata e non il Kernel. Quando intenderò il kernel, se necessario, aggiungerò la parola "Kernel" davanti a Linux.

### Come mai GNU-Linux?

- Si tratta di un sistema FOSS (free and open source), codice controllabile e controllato, senza lo spyware aggiunto di Windows o MacOS. Immagina effettivamente possedere il computer che hai comprato (_insert cyberpunk dystopia here_).
- Aggiorni il sistema quando vuoi: se ti piace restare alla versione di 6 anni fa oppure non aggiornare mai una certa applicazione, Linux non lo farà per te a meno che non direttamente istruito. Inoltre, (molto probabilmente) non dovrai riavviare immediatamente dopo un aggiornamento di sistema (vd. Rolling Release distributions).
- Sia il kernel, sia i sistemi che si appoggiano al kernel, sono estremamente sicuri. Così sicuri ed efficienti che il 99% del web si appoggia a Linux. Antivirus? Mai sentito.
- Una corretta separazione root-user.
- _Drivers_? Quali _drivers_? Una distribuzione Linux funziona nella stragrande maggioranza dei casi senza l'installazione di drivers esterni: il kernel pensa a tutto.
- Leggero, veloce: immagina avere un sistema operativo senza candy crush saga pre-installato.
- Estremamente personalizzabile: così personalizzabile che puoi far saltare tutto in aria con un comando.
- Tante, tante, tante [distribuzioni](https://distrowatch.com/)...

## Intro a Linux: le distribuzioni

### La scelta della distro perfetta

Partiamo subito su un tema caldissimo del mondo Linux: quale distribuzione mi conviene scegliere?
Darò per scontato che tu sia alle prime armi e non abbia mai usato Linux se non in una macchina virtuale (molto, molto male! Ora rimediamo).
La scelta della distribuzione è importante, ma non fondamentale. Linux rimane sempre Linux, e quindi in ogni caso è sempre possibile installare qualsiasi applicazione
compatibile con una distribuzione su un'altra.

Ciò che distingue quindi una distro da un'altra sono i programmi pre-installati, solitamente.
Il mondo di Linux è perciò diviso tra una scelta di software e configurazioni, tanto da perderci la testa.
Andiamo adesso, passo passo, a vedere cosa differenzia una distribuzione dall'altra.

### Il package manager

Al contrario di Windows, Linux non ha bisogno degli eseguibili. Tutto viene installato attraverso un package manager.
Questo ci da la convenienza di poter aggiornare tutte le applicazioni insieme oppure a ritmi diversi, se necessario.

[Qui](https://itsfoss.com/package-manager/) una guida che mostra cosa sia un package manager.

Di Package Managers ne esistono tantissimi, ma i più famosi sono apt (Advanced Package Tool, tipico delle distribuzioni Ubuntu-based),
pacman (tipico delle distribuzioni arch-based), dnf (Dandified Yum, il successore di rpm, il package manager di Fedora), portage (Gentoo) e Zypper (openSUSE).

Scegliere il package manager vuol dire scegliere il modo in cui verranno installate **tutte** le vostre applicazioni.
Le differenze nella gestione dei pacchetti probabilmente fanno più la differenza per gli amministratori della distribuzione Linux che per gli utenti finali. 
Il compito di mantenere la repository dei pacchetti della tua distribuzione Linux sana non è un compito banale o facile. Se una voce del database di un pacchetto contiene un errore, chiunque nel mondo utilizzi la tua distribuzione improvvisamente non sarà in grado di installare quel pacchetto o, peggio, installarlo potrebbe danneggiare il sistema.
Questo accadeva spesso con Gentoo Linux all'inizio. C'erano volte in cui provavo a installare qualcosa di cui avevo veramente bisogno, solo per scoprire che c'era un errore nel database del pacchetto e l'installazione non sarebbe riuscita, e avrei dovuto dire loro dell'errore, aspettare qualche giorno prima che lo risolvessero, quindi provare a installarlo di nuovo. Questo non è più un problema così tanto, ma le funzionalità del gestore di pacchetti aiutano gli amministratori del database a mantenere le cose senza intoppi e a prevenire il verificarsi di errori.

Per gli utenti finali, non c'è quasi alcuna differenza, perché ogni gestore di pacchetti deve fare lo stesso lavoro e lo fanno tutti in modi leggermente diversi. Devono tutti connettersi a un repository attendibile e scaricare e verificare l'autenticità dei pacchetti firmati crittograficamente, tutti devono avere informazioni su quali pacchetti dipendono da cosa altri e tutti hanno bisogno di consentirti di installare e disinstallare roba sul tuo sistema senza rompere le cose.

Ora uno potrebbe certamente dire che _pacman_ sia più veloce o _apt_ più reliable, ma sono chiacchericci.
Nella sostanza, quello che cambia sono i comandi per chiamarli, che sono riassunti [qui](https://distrowatch.com/dwres.php?resource=package-management).

### Il Desktop Environment

Un DE raggruppa una varietà di componenti per fornire elementi comuni dell'interfaccia utente grafica come icone, barre degli strumenti, sfondi e widget desktop. Inoltre, la maggior parte dei DE include una serie di applicazioni già integrate. Soprattutto, i DE dispongono di un proprio window manager, che tuttavia può essere generalmente sostituito con un altro compatibile.

L'utente è libero di configurare il proprio ambiente GUI in qualsiasi modo. Gli ambienti desktop forniscono semplicemente un mezzo completo e conveniente per aiutarci in questa attività. Tieni presente che gli utenti sono liberi di combinare applicazioni da più ambienti desktop. Ad esempio, un utente di **KDE** può installare ed eseguire applicazioni **GNOME** come il browser Web Epiphany, se lo preferisce al browser Web Konqueror di KDE. Uno svantaggio di questo approccio è che molte applicazioni fornite da progetti di ambiente desktop fanno molto affidamento sulle librerie alla base del rispettivo ambiente desktop. Di conseguenza, l'installazione di applicazioni da una vasta gamma di ambienti desktop richiederà l'installazione di un numero maggiore di dipendenze. Gli utenti che cercano di risparmiare spazio su disco spesso evitano tali ambienti misti o scelgono alternative che dipendono solo da poche librerie esterne.

Inoltre, le applicazioni fornite dagli ambienti desktop tendono a integrarsi meglio con i loro ambienti nativi. Parlando di visibilità, la combinazione di ambienti con diversi toolkit di widget risulterà in discrepanze visive (ovvero, le interfacce utilizzeranno icone e stili di widget diversi). In termini di usabilità, gli ambienti misti potrebbero non comportarsi in modo simile (ad es. clic singolo o doppio clic sulle icone; funzionalità di trascinamento...) causando potenzialmente confusione o comportamenti imprevisti.

I DE più utilizzati ed amati sono:

- BUDGIE
- CINNAMON
- CUTEFISH
- DEEPIN
- **GNOME**
- **KDE**
- **LXDE**
- LXQT
- MATE
- UKUI
- **XFCE**

In grassetto ho sottolineato quelli a mio parere meglio supportati dalle applicazioni.

### Distro Tier List

Qua di seguito ho voluto raggruppare sotto forma di Tier List una serie di distribuzioni che ritengo utilizzabili da uno studente di informatica.
Sono organizzate dalle migliori alle peggiori in termini di usabilità, bellezza, sicurezza, velocità, utilizzo day-by-day, gaming ed altri fattori.
Tra gli altri fattori ho personalmente contato l'inclusione di blob proprietari (vd. Snap per Canonical...), che vanno contro la filosofia UNIX.
Dentro il singolo tier, le distro non sono in un particolare ordine, giusto per triggherare il vostro OCD.

#### S-Tier

- Pop_OS
- Nobara Linux
- ArcoLinux (advanced)

#### A-Tier

- Fedora
- Linux Mint
- Xubuntu
- MX Linux
- NixOS (advanced)
- Artix (advanced)
- Archcraft (advanced)
- KISS (very, very, very advanced)

#### B-Tier

- Ubuntu
- Lubuntu
- EndeavourOS
- RebornOS
- Garuda Linux
- Nitrux
- ZorinOS
- CutefishOS
- Kubuntu
- Elementary OS
- Alpine (advanced)

#### C-Tier (da evitare)

- Manjaro
- Deepin

#### ?-Tier (non installarle a meno che tu non sia effettivamente a conoscenza del perché di queste distro)

- Parrot
- Kali Linux
- Qubes OS
- Tails

#### Ottima ma ancora in beta

- VanillaOS
- InstantOS (advanced)

## Come installare la mia distribuzione?

### Di cosa ho bisogno?

Una chiavetta, un computer BARE METAL (non una VM, quindi) su cui installare la distro, un computer da cui burnare la ISO, alcuni tweak del BIOS.

### Pre: tweak del bios

- Disattiva secure-boot e tutte quelle boiate come il TPM.
- Disattiva SATA operation in modalità RAID
- Disattiva Fast Boot

### Software per burnare la ISO consigliati

- Balena Etcher (very yes, much consigliato)
- UNetbootin (yes.)
- LinuxLiveUSB-Creator (anche se il progetto non è più mantenuto)
- Rufus (anche se personalmente ho avuto diversi problemi con questo)

## Quali applicazioni mi consigli non appena ho finito con l'installazione?

- Bottles (Runnare le app di Windows)
- Firefox (Browser web)
- Fish (Shell basata)
- Flatseal (Per gestire i permessi delle app Flatpak)
- GParted (Per gestire le partizioni)
- GIMP (Immagini)
- HandBrake (Per gestire file video, comprimerli, tagliarli...)
- Neovim (HELL YEAH.)
- OBS Studio (Per fare screen recording)
- Obsidian (Per prendere appunti)
- Sioyek (Per leggere paper accademici)
- Steam (Gaming)
- Thunderbird (Per le mail)
- Visual Studio Code (Per il codice)
- VLC (Per video ed audio)

### App basatissime che potrebbero aprirti la mente

- Librewolf
- aria2
- youtube-dlp
- qrcp
- rsync
- Cryptomator
- FileZilla
- peerflix
- Kitty
- RustDesk
- qimgv
- ImageMagick
- oxipng e Guetzli
- Flameshot
- discord-screenaudio

### Non trovo l'app X che invece avevo su Windows, come faccio?

[Tiè](https://wiki.archlinux.org/title/List_of_applications).

## Ma su Windows potevo fare X e invece su Linux non posso, e ora?

[Clicca qui](https://www.youtube.com/watch?v=KkLwnCKkf5I).

## Si, però, su Windows potevo giocare ad X senza troppi problemi

Usa Lutris. git good.

## Si, però, mio cuggino mi ha detto che Linux ha un sacco di problemi e non va bene!!1

Scusa? Non ti sento! Ho gcc che compila in background nativamente mentre il tuo 20% di CPU è impegnato a mandare dati a Microsoft o Apple.

## Scimmie Napoletane

[Scimmie Napoletane](https://www.youtube.com/watch?v=L5otNdhaeeI).
