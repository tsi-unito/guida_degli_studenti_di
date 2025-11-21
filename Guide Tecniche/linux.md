# La guida all-in-one al mondo GNU-Linux @ UniTO (WIP!)

## Changelog

- 21-11-2025: Aggiornamento della guida
- 13-11-2024: Revisione completa della guida, rimossa tier list
- 24-10-2023: update alla tier list

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
- Una corretta separazione root-user: niente più "Esegui come amministratore" ogni 3 secondi.
- _Drivers_? Quali _drivers_? Una distribuzione Linux funziona nella stragrande maggioranza dei casi senza l'installazione di drivers esterni: il kernel pensa a tutto.
- Leggero, veloce: immagina avere un sistema operativo senza candy crush saga pre-installato.
- Estremamente personalizzabile: così personalizzabile che puoi far saltare tutto in aria con un comando.
- Tante, tante, tante [distribuzioni](https://distrowatch.com/)... ma tranquillo, ti aiuto io a scegliere.

## Intro a Linux: le distribuzioni

### La scelta della distro perfetta

Partiamo subito su un tema caldissimo del mondo Linux: quale distribuzione mi conviene scegliere?
Darò per scontato che tu sia alle prime armi e non abbia mai usato Linux se non in una macchina virtuale (molto, molto male! Ora rimediamo).
La scelta della distribuzione è importante, ma non fondamentale. Linux rimane sempre Linux, e quindi in ogni caso è sempre possibile installare qualsiasi applicazione
compatibile con una distribuzione su un'altra.

### Distribuzioni consigliate

Per iniziare, ecco quattro distribuzioni particolarmente indicate per chi si avvicina a Linux:

- **Linux Mint**: Perfetta per chi viene da Windows, stabile e user-friendly. Basata su Ubuntu ma senza i problemi legati agli Snap packages. Mint è tipo Ubuntu ma senza le boiate di Canonical. Viene con Cinnamon come DE di default, che è praticamente Windows ma fatto bene.

- **Pop!_OS**: Ideale per programmatori e gamers che non vogliono farsi tante domande. Offre driver proprietari NVIDIA preinstallati e un'esperienza out-of-the-box eccellente. Il suo DE è basato su GNOME ma con modifiche che lo rendono effettivamente utilizzabile. Ottima per laptop con doppia GPU. Nel caso in cui qualcuno menzioni il fatto che *gne gne, non viene aggiornata da tot anni*, **COSMIC** sta arrivando.

- **Fedora**: Un ottimo compromesso tra stabilità e innovazione. Perfetta per sviluppatori e per chi vuole restare aggiornato senza sacrificare l'affidabilità. GNOME vanilla experience, ma fatta bene. Red Hat ci mette i soldi, quindi sai che non morirà domani.

- **Debian**: La "madre" di molte distribuzioni. Estremamente stabile e affidabile, ideale per chi cerca un sistema solido che "semplicemente funziona". La usano i server di mezzo mondo, quindi sai che è stabile. Un po' old school nell'approccio, ma hey, funziona.

## Desktop Environment (DE) e Window Manager (WM)

### Cosa sono?

Il DE è l'interfaccia grafica che vedi quando usi il computer. È composto da:
- Un window manager (WM) che gestisce le finestre
- Un set di applicazioni base (file manager, terminale, etc.)
- Un set di librerie grafiche
- Un insieme di temi e icone

Il WM invece è solo il gestore delle finestre. Può essere:
- **Floating**: le finestre "galleggiano" sullo schermo (come in Windows)
- **Tiling**: le finestre sono organizzate in modo da occupare tutto lo spazio disponibile
- **Dynamic**: un mix dei due precedenti
- **Scrollable**: le finestre sono organizzate in una striscia orizzontale infinita.

### DE Consigliati

- **GNOME**: Moderno, minimalista, touch-friendly. Pesante come un mattone ma bello da vedere. Usa tantissima RAM ma almeno è carino.
  - Pro: Bellissimo, coerente, facile da usare
  - Contro: Pesante, poco personalizzabile (senza estensioni)

- **KDE Plasma**: Potente, personalizzabile, leggero. Windows-like fatto bene.
  - Pro: Super personalizzabile
  - Contro: A volte troppo personalizzabile, può spaventare i novizi

- **Cinnamon**: Il DE di Linux Mint. Classico, stabile, familiare.
  - Pro: Facile da usare, stabile, non ti fa impazzire
  - Contro: Un po' noioso, non molto moderno

- **XFCE**: Leggero, stabile, tradizionale. Per PC vecchi, per chi vuole velocità o Bini.
  - Pro: Velocissimo, stabile come una roccia
  - Contro: Non bellissimo di default, richiede un po' di setup

### WM Standalone

Se sei un power user, potresti voler usare solo un WM. 
Ecco i più popolari:

+ **X11**:
- **i3**: Tiling WM leggero e configurabile. La porta d'ingresso nel mondo dei tiling WM.
- **DWM**: Minimalista, veloce, da configurare in C. Per masochisti.

- **AwesomeWM**: Potente e programmabile in Lua. Per veri nerd.
- **Xmonad**: cheat per passare LPP.
+ **Wayland**:
- **Sway**: Come i3 ma per Wayland. Il futuro è qui.
- **Niri**: Scrollable-tiling, veramente fantastico.

## Package Manager e Software

### APT (Debian/Ubuntu & derivate)

```bash
sudo apt update             # Aggiorna la lista dei pacchetti
sudo apt upgrade           # Aggiorna i pacchetti installati
sudo apt install package   # Installa un pacchetto
sudo apt remove package    # Rimuove un pacchetto
sudo apt autoremove       # Rimuove le dipendenze non più necessarie
```

### DNF (Fedora/RHEL & derivate)

```bash
sudo dnf update            # Aggiorna tutto
sudo dnf install package   # Installa un pacchetto
sudo dnf remove package    # Rimuove un pacchetto
sudo dnf autoremove       # Rimuove le dipendenze non più necessarie
```

### Pacman (Arch & derivate)

```bash
sudo pacman -Syu          # Aggiorna tutto
sudo pacman -S package    # Installa un pacchetto
sudo pacman -R package    # Rimuove un pacchetto
sudo pacman -Rns package  # Rimuove un pacchetto e le sue dipendenze
```

## Software Essenziale

### Browser
- **Firefox**: Il browser FOSS per eccellenza
- **Thorium**: Chrome ma senza Google che ti spia
- **Zen**: Firefox, but better.

### Editor di Testo & IDE
- **VSCode**: L'editor che tutti conoscono (usa la versione OSS)
- **Neovim**: Per chi odia il mouse e ama la propria sanità mentale, ottimo ecosistem di plugins 
- **Helix**: editor modale scritto in rust, simile a vim/Neovim ma con molte features out-of-the-box
- **GNOME Text Editor**: Semplice ma efficace
- **Kate**: Come notepad++ ma fatto bene
- **JetBrains**: Per chi ha RAM da bruciare (e una licenza universitaria)

### Terminal Emulator
- **GNOME Terminal**: Viene con GNOME, fa il suo lavoro
- **Konsole**: Il terminale di KDE, veloce e personalizzabile
- **Alacritty**: Veloce, GPU-accelerated, configurabile in YAML
- **Kitty**: Come Alacritty ma con più feature
- **foot**: Rust-based, carino da utilizzare
- **WezTerm**: Rust-based, veloce, personalizzabile

### Shell
- **Bash**: Il default, funziona
- **Zsh**: Bash++ con plugin (oh-my-zsh ftw)
- **Fish**: User friendly, default sane, non POSIX, _il mio preferito_
- **Nushell**: Per chi vuole qualcosa di completamente diverso

### Utility
- **htop/btop**: Task manager ma bello
- **bottom**: versione potenziata dei vari top
- **fastfetch**: Per flex su r/unixporn
- **timeshift**: Backup del sistema, salvati la vita
- **flameshot**: Screenshot tool potente
- **bat**: cat ma con syntax highlighting
- **eza/lsd**: ls ma decente
- **fzf**: Fuzzy finder per il terminale
- **television**: TUI per Fuzzy finder
- **gitui**: TUI per gestire i comandi di git
- **ripgrep**: grep ma più veloce

## Tips & Tricks

### Shell Aliases Utili

```bash
# Metti nel tuo .bashrc o .zshrc
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias update='sudo apt update && sudo apt upgrade' # o dnf/pacman
alias please='sudo $(history -p !!)'
alias cls='clear'
alias cd..='cd ..'
```

### Comandi Utili

```bash
# Spazio disco
df -h               # Spazio disco per partizione
du -sh *           # Spazio usato per cartella

# Processi
ps aux | grep nome  # Cerca processo
kill -9 PID         # Termina processo

# Network
ip a                # Info network
ss -tunlp          # Porte aperte
ping google.com     # Test connessione

# File
find . -name "*.txt"  # Cerca file
locate filename       # Cerca file (database)
grep -r "testo" .     # Cerca testo nei file
```

### Permessi e Proprietà

```bash
chmod +x script.sh     # Rendi eseguibile
chown user:group file  # Cambia proprietario
sudo !!               # Ripeti ultimo comando con sudo
```

## Troubleshooting Comune

### Wi-Fi non funziona

1. sudo rfkill list per vedere se bloccato
2. sudo rfkill unblock all per sbloccare
3. Controlla driver con lspci -k

### Scheda video NVIDIA

1. Usa driver proprietari se possibile
2. Pop!_OS ha già tutto configurato
3. prime-select per switchare GPU

### Sistema non si avvia

1. Modalità recovery dal GRUB/systemd-boot
2. fsck per check filesystem
3. timeshift per restore backup

### Altri problemi

Controlla la Arch Wiki.

## Risorse & Link Utili

### Community & Supporto
- [Reddit r/linux4noobs](https://reddit.com/r/linux4noobs) - Per domande da novizi
- [Reddit r/unixporn](https://reddit.com/r/unixporn) - Per flex e ispirazione
- [AskUbuntu](https://askubuntu.com/) - StackOverflow per Ubuntu
- [Arch Wiki](https://wiki.archlinux.org/) - La bibbia di Linux, non solo per Arch

### Tutorial & Guide
- [Linux Journey](https://linuxjourney.com/) - Tutorial interattivo
- [The Linux Command Line](https://linuxcommand.org/tlcl.php) - Libro gratuito
- [Greg's Wiki](https://mywiki.wooledge.org/BashGuide) - Guida Bash definitiva

### Tools Online
- [Repology](https://repology.org/) - Cerca pacchetti tra distro
- [AlternativeTo](https://alternativeto.net/) - Alternative FOSS a software proprietario
- [TLDRPages](https://tldr.sh/) - man pages semplificate

## Pro Tips

### Sicurezza Base
```bash
# Firewall
sudo ufw enable
sudo ufw default deny incoming
sudo ufw default allow outgoing
sudo ufw allow ssh

# Updates automatici (Ubuntu/Debian)
sudo apt install unattended-upgrades
sudo dpkg-reconfigure unattended-upgrades
```
## Come installare la mia distribuzione?

### Di cosa ho bisogno?

Una chiavetta, un computer BARE METAL (non una VM, quindi) su cui installare la distro, un computer da cui burnare la ISO, alcuni tweak del BIOS.

### Pre: tweak del bios

- Disattiva secure-boot e tutte quelle boiate come il TPM.
- Disattiva SATA operation in modalità RAID
- Disattiva Fast Boot

### Software per burnare la ISO consigliati

- Popsicle (il migliore, ma non esiste per Windows/MacOS)
- Balena Etcher (very yes, much consigliato)
- Rufus (anche se personalmente ho avuto diversi problemi con questo)

### App basatissime che potrebbero aprirti la mente

- aria2 (Cli torrent client)
- youtube-dlp (youtube-dl ma meglio)
- qrcp (Trasferimento file da telefono a pc reso facile)
- rsync (Il programma di Dio)
- Cryptomator (Zero-trust encryption per il cloud)
- FileZilla (FTP made easy)
- Stremio (shh...)
- rmpc (per la musica) 
- yazi (terminal file manager)
- RustDesk (TeamViewer ma FOSS)
- qimgv (Visualizzatore di immagini veloce e leggero)
- ImageMagick (Modifica di immagini, programma FONDAMENTALE)
- Flameshot (Screenshot)
- discord-screenaudio

## Informazioni importanti subito dopo l'installazione di una qualsiasi distribuzione Linux

1. Update e riavvia: non tutte le iso scaricate sono aggiornate, anzi!
2. Installa un host blocker, come [hblock](https://github.com/hectorm/hblock).

## FAQ

### Non trovo l'app X che invece avevo su Windows, come faccio?

[Tiè](https://wiki.archlinux.org/title/List_of_applications).

### Ma su Windows potevo fare X e invece su Linux non posso, e ora?

[Clicca qui](https://www.youtube.com/watch?v=KkLwnCKkf5I).

### Si, però, su Windows potevo giocare ad X senza troppi problemi

Usa Lutris. git gud.

Inoltre è presente heroic launcher che permette di gestire le varie versioni di wine e forisce un punto in cui raccogliere i propri giochi.

### Si, però, mio cuggino mi ha detto che Linux ha un sacco di problemi e non va bene!!1

Scusa? Non ti sento! Ho gcc che compila in background nativamente mentre il tuo 20% di CPU è impegnato a mandare dati a Microsoft o Apple.

### Scimmie Napoletane

[Scimmie Napoletane](https://www.youtube.com/watch?v=L5otNdhaeeI).
