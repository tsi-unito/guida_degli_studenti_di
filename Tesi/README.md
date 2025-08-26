# Guida alla scrittura della tesi

## Primo passo: imparare LaTeX o Typst

### LaTeX

Quando ci si approccia alla scrittura della tesi è essenziale avere gli strumenti corretti per scriverla.
Molti professori richiedono esplicitamente di leggere la tesi in LaTeX, per diversi motivi:

1. LaTeX rappresenta uno standard per la comunità scientifica internazionale, un po' come l'inglese per le lingue
2. LaTeX è un linguaggio relativamente facile da imparare, puoi farlo letteralmente in [30 min](https://www.overleaf.com/learn/latex/Learn_LaTeX_in_30_minutes)!
3. LaTeX è, al contrario dei classici Word Processors, intercompatibile con qualsiasi LaTeX reader e sistema operativo possibile!
4. Il font default di LaTeX è Computern Modern, il leggendario font creato da Knuth

Prima di iniziare, se vuoi spendere ancora due minuti, dai un'occhiata alla storia di [come è nato](https://en.wikipedia.org/wiki/LaTeX#History) LaTeX!

### Typst

[Typst](https://github.com/typst/typst) è un nuovo sistema di typesetting che mira a mantenere la flessibilità di LaTeX, ma con una sintassi più moderna ed un linguaggio di scripting più potente.

## Templates

### LaTeX

Fondamentale per iniziare a lavorare su LaTeX è cercare qualche template decente per la tua tesi. In questa cartella troverai delle baseline per i frontespizi,
anche se personalmente consiglio di iniziare con i [templates di OverLeaf](https://www.overleaf.com/latex/templates/)!
Da qui, la strada è in discesa: modifica, ritaglia ed incolla le parti che ti piacciono o meno.

### Typst

Per iniziare a scrivere la propria tesi su Typst, è possibile scrivere da se il proprio template o utilizzare il template linkato in [template-tesi-typst](https://github.com/eduardz1/UniTO-typst-template) ([pagina universe](https://typst.app/universe/package/modern-unito-thesis)).

Per inizializzare un progetto con il template menzionato è possibile clonare [la repo](https://github.com/eduardz1/unito-typst-template) o eseguire da CLI il comando:

```bash
typst init @preview/modern-unito-thesis:0.1.0
```

## Editor

### LaTeX

Qua è una scelta personale: moltissimi professori utilizzano [OverLeaf](https://www.overleaf.com/project), un editor LaTeX online semplice ed efficace.
Niente toglie che possiate utilizzare il vostro editor preferito: VSC, (n)vim e quant'altro, se correttamente configurati, possono essere utilissimi.

### Typst

Così come per LaTeX, Typst offre un [editor collaborativo online](https://typst.app/).

In alternativa è possibile utilizzare un editor locale, l'LSP integrato [Tinymist](https://github.com/Myriad-Dreamin/tinymist) offre integrazioni per VSCode, Neovim, Helix e altri editor.

## Simboli frequentemente utilizzati

### LaTeX

Potete trovare delle shortcut utilissime in [questo PDF](https://www.evilscript.eu/files/symbols.pdf) riguardante i simboli frequentemente utilizzati in LaTeX.

### Typst

In VSCode, con l'estensione di Tinymist, è possibile cercare direttamente simboli dall'editor, sia per nome che disenando il simbolo stesso.

In ogni caso consiglio sempre di fare riferimento alla [documentazione ufficiale di Typst](https://typst.app/docs) dove ogni simbolo può essere cercato anche con il suo nome in LaTeX.

## Spazi e spazietti in LaTeX

[Qua](https://www.sascha-frank.com/spacing.html), invece, troverete una guida su come utilizzare spazi e tab in LaTeX.

## Tabelle

### LaTeX

Su [questo sito](https://www.tablesgenerator.com/latex_tables), troverete un editor di tabelle LaTeX per non impazzire durante la scrittura della vostra tesi.

### Typst

Le tabelle in Typst, al contrario di LaTeX, sono già molto feature complete di base e non richiedono l'uso di [pacchetti esterni](https://typst.app/universe) (che comunque sono disponibili).

Fare riferimento a [questa sezione](https://typst.app/docs/guides/table-guide/) del manuale.

## Bibliografia incasinata?

Utilizzate [questo tool](https://flamingtempura.github.io/bibtex-tidy/) per pulire il vostro BibTeX e formattarlo adeguatamente!
