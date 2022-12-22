---
title: 'Basi di dati - Esercizi laboratorio'
---

# Basi di Dati - Esercizi laboratorio

## Interrogazioni di base

### Esercizio 2.3

- Quali sono le parti disponibili nella stessa città di un fornitore (non necessariamente fornite da quel fornitore)? Elencare nome, colore e peso delle parti e nome del fornitore.

```sql
SELECT pname, color, weight, sname FROM s, p WHERE s.city = p.city;
```

### Esercizio 2.4

- Elencare, in ordine alfabetico crescente, il nome dei fornitori che hanno evaso almeno una volta ordini di almeno $300$ bulloni (*bolt*) o di almeno $300$ dadi (*nut*).

```sql
SELECT DISTINCT s.sname
FROM s, sp, p
WHERE s.snum = sp.snum AND sp.pnum = p.pnum AND sp.qty >= 300 AND (p.pname = 'Bolt' OR p.pname = 'Nut')
ORDER BY s.sname;
```

### Esercizio 2.5

- Elencare, in ordine di status decrescente, i fornitori che hanno fornito parti con peso fuori dall’intervallo $[14, 17]$ kg.
    - Mostrare il nome del fornitore e il suo status.

```sql
SELECT DISTINCT s.sname, s.status
FROM s, sp, p
WHERE s.snum = sp.snum AND sp.pnum = p.pnum AND NOT (p.weight >= 14 AND p.weight <= 17)
ORDER BY s.status DESC;
```

-----

## Join

### Esempio 1 - Self join

- Elencare le coppie di fornitori appartenenti alla stessa città e la loro città.

```sql
SELECT DISTINCT F1.SNum Fornitore1, F2.SNum Fornitore2, F1.City
FROM S F1 join S F2 on F1.City = F2.City
WHERE F1.SNum < F2.SNum;
```

- `<` è una clausola più restrittiva di `<>`.
    - L'utilizzo di `<>` avrebbe generato la tupla $(s1, s4)$, ma anche $(s4, s1)$ (errore).

### Esercizio 3.1

- Elencare *tutti* i fornitori con status superiore a $20$ e la quantità delle parti *eventualmente* fornite.

```sql
SELECT s.*, sp.qty
FROM s LEFT JOIN sp ON s.snum = sp.snum
WHERE s.status > 20
```

### Esercizio 3.2

- Elencare i nomi di tutte le parti di colore verde e le città dei loro eventuali fornitori.

```sql
SELECT p.pname, s.city
FROM p LEFT JOIN sp ON p.pnum = sp.pnum LEFT JOIN s ON sp.snum = s.snum
WHERE p.color = 'Green';
```

- Non mettendo un `LEFT JOIN` nel secondo `JOIN` si annullerebbe in automatico il primo.

### Esercizio 3.3

- Elencare tutti i fornitori che hanno forniture minori di $200$ parti (e quindi anche i fornitori che non hanno fornito nulla).
    - Il risultato deve comprendere il nome del fornitore e la quantità delle parti eventualmente fornite.

```sql
SELECT DISTINCT s.sname, sp.qty
FROM s LEFT JOIN sp ON s.snum = sp.snum
WHERE sp.qty < 200 OR sp.qty IS NULL;
```

### Esercizio 3.4

- Elencare tutte le coppie di parti disponibili nella stessa città ma di colore diverso.
    - Mostrare codice delle parti e nome della città.

```sql
SELECT p1.pnum, p2.pnum, p2.city
FROM p p1 JOIN p p2 ON p1.city = p2.city
WHERE p1.pnum < p2.pnum AND p1.color <> p2.color;
```

- Bisogna imporre un ordinamento restrittivo (`p1.pnum < p2.pnum`) per evitare di avere coppie con stessi elementi ordinati differentemente.

### Esercizio 3.5

- Elencare tutte le coppie di parti fornite dallo stesso fornitore.
    - Mostrare nome del fornitore, codice e nome delle parti.

```sql
SELECT sname, p1.pnum, p1.pname, p2.pnum, p2.pname
FROM sp sp1 JOIN sp sp2 ON sp2.snum = sp1.snum 
    JOIN p p1 ON sp1.pnum = p1.pnum
    JOIN p p2 ON sp2.pnum = p2.pnum
    JOIN s ON sp2.snum = s.snum
WHERE p1.pnum < p2.pnum;
```

-----

## Funzioni aggregate e raggruppamento

### Esempio 1 - Raggruppamento per più attributi

- Estrarre il numero di fornitori per città e status con status minimo $10$.

```sql
SELECT city, status, COUNT(snum)
FROM s
WHERE status >= 10
GROUP BY city, status;
```

### Esempio 2 - Conteggio

- Estrarre il numero di fornitori per città con status di almeno $20$.

```sql
SELECT city, COUNT(snum)
FROM s
WHERE status >= 20
GROUP BY city;
```

- Elencare le città con almeno due fornitori.

```sql
SELECT city
from s
GROUP BY city
HAVING COUNT(*) >= 2
```

### Esercizio 4.1

- Estrarre la quantità totale di parti rosse fornite da ciascun fornitore.
    - Mostrare nome del fornitore e quantità totale di parti.

```sql
SELECT s.sname, SUM(sp.qty)
FROM s JOIN sp ON s.snum = sp.snum JOIN p ON sp.pnum = p.pnum
WHERE p.color = 'Red'
GROUP BY s.snum, s.sname;
```

- Estrarre la quantità totale di parti rosse fornite da ciascun fornitore compresi i fornitori che non forniscono nessuna parte.
    - Per quest'ultimi mostrare come quantità $0$.

```sql
SELECT s.sname, COALESCE(SUM(sp.qty), 0)
FROM s LEFT JOIN sp ON s.snum = sp.snum LEFT JOIN p ON sp.pnum = p.pnum
WHERE p.color = 'Red' OR p.color IS NULL
GROUP BY s.snum, s.sname;
```

- Estrarre la quantità totale di parti rosse fornite da ciascun fornitore compresi i fornitori che non forniscono nessuna parte rossa.

```sql
SELECT sname, COALESCE(SUM(qty), 0)
FROM s LEFT JOIN (sp JOIN p ON sp.pnum = p.pnum) ON s.snum = sp.snum AND color = 'Red'
GROUP BY sp.snum, s.sname;
```

### Esercizio 4.2

- Considerando solo forniture di oltre $100$ parti, estrarre le città in cui i fornitori, considerati insieme, forniscono in totale almeno $1000$ parti.
    - Mostrare la città e la quantità totale di parti.

```sql
SELECT s.city, SUM(qty)
FROM s JOIN sp ON (s.snum = sp.snum)
WHERE sp.qty > 100
GROUP BY s.city
HAVING SUM(qty) >= 1000;
```

### Esercizio 4.3

- Estrarre le città in cui ci sono almeno due fornitori che hanno fornito ognuno almeno due prodotti di diverso colore.

```sql
SELECT s.city
FROM sp sp1
    JOIN sp sp2 ON sp1.snum = sp2.snum
    JOIN p p1 ON sp1.pnum = p1.pnum
    JOIN p p2 ON sp2.pnum = p2.pnum
    JOIN s ON sp1.snum = s.snum
WHERE
    p1.color < p2.color
GROUP BY s.city
HAVING COUNT(DISTINCT s.snum) >= 2;
```

-----

## Operatori insiemistici e sottoquery semplici

### Esempio 1 - Intersezione

- Trovare i codici dei fornitori che hanno fornito almeno un prodotto di colore rosso e uno verde.

```sql
SELECT sp.snum
FROM sp JOIN p ON sp.pnum = p.pnum
WHERE color = 'Red'
INTERSECT
SELECT sp.snum
FROM sp JOIN p ON sp.pnum = p.pnum
WHERE color = 'Green';
```

- Soluzione senza l'utilizzo dell'operatore insiemistico.

```sql
SELECT DISTINCT sp1.snum
FROM sp sp1 JOIN p p1 ON sp1.pnum = p1.pnum, sp sp2 JOIN p p2 ON sp2.pnum = p2.pnum
WHERE sp1.snum = sp2.snum AND p1.color = 'Red' AND p2.color = 'Green';
```

### Esempio 2 - Differenza insiemistica

- Trovare i codici dei fornitori che hanno fornito almeno un prodotto di colore rosso ma non uno di colore verde.

```sql
SELECT Sp.snum
FROM sp JOIN p ON sp.pnum = p.pnum
WHERE color = 'Red'
EXCEPT
SELECT sp.snum
FROM sp JOIN p ON sp.pnum = p.pnum
WHERE color = 'Green';
```

- Non è possibile non usare un operatore insiemistico, bisogna utilizzare delle query nidificate.

### Esempio 3 - `IN` e `NOT IN`

- Elencare i fornitori che non hanno fornito parti blu.

```sql
SELECT DISTINCT snum FROM s
WHERE snum NOT IN
    (SELECT snum FROM sp
     WHERE pnum IN
        (SELECT pnum FROM p
         WHERE color = 'Blue'));
```

- Elencare per ogni fornitore la quantità massima fornita.

```sql
SELECT s.*, COALESCE(qty, 0)
FROM s LEFT JOIN sp ON s.snum = sp.snum
WHERE sp.snum IS NULL OR (sp.snum, qty) in
    (SELECT snum, MAX(qty)
     FROM sp
     GROUP BY snum);
```

### Esempio 4 - Sottoquery nella clausola `FROM`

- Elencare per ogni fornitore la quantità massima fornita.

```sql
SELECT sname, COALESCE(maxQty, 0)
FROM s LEFT JOIN
    (SELECT snum, MAX(qty) AS maxQty
     FROM sp
     GROUP by snum) smax
ON s.snum = smax.snum;
```

### Esempio 5 - Sottoquery nella clausola `SELECT`

- Elencare i fornitori e il numero totale di parti fornite.

```sql
SELECT snum, COLASCE((SELECT SUM(qty)
              FROM sp
              WHERE sp.snum = s.snum), 0)
FROM s;
```

### Esercizio 5.1

- Elencare i fornitori che forniscono parti disponibili a Londra.
    - Sia con costrutto `IN`/`NOT IN` che con costrutto `ANY`/`ALL`.

```sql
SELECT DISTINCT snum
FROM sp
WHERE pnum IN
    (SELECT pnum
     FROM p
     WHERE city = 'London');
```

```sql
SELECT DISTINCT snum
FROM sp
WHERE pnum = ANY
    (SELECT pnum
     FROM p
     WHERE city = 'London');
```

### Esercizio 5.2

- Elencare le città in cui non vi sono fornitori con status minore della media.
    - Sia con costrutto `IN`/`NOT IN` che con costrutto `ANY`/`ALL`.

```sql
SELECT DISTINCT city
FROM s
WHERE city NOT IN
    (SELECT city
     FROM s
     WHERE status <
        (SELECT AVG(status) FROM s));
```

```sql
SELECT DISTINCT city
FROM s
WHERE city <> ALL
    (SELECT city
     FROM s
     WHERE status <
        (SELECT AVG(status) FROM s));
```

-----

## Sottoquery correlate

### Esempio 1

- Elencare i fornitori nelle cui città sono disponibili almeno due prodotti.

```sql
SELECT snum, city
FROM s
WHERE 2 <=
    (SELECT COUNT(*)
     FROM p
     WHERE p.city = s.city);
```

- Soluzione senza l'utilizzo di sottoquery correlata.

```sql
SELECT s.snum, s.city
FROM s JOIN p ON s.city = p.city
GROUP BY s.city, s.snum
HAVING COUNT(*) >= 2;
```

### Esercizio 6.1

- Trovare i codici dei prodotti che hanno il peso massimo.
    - Scrivere una versione determinando il peso massimo come il peso non inferiore ai pesi di tutti gli altri prodotti.
    - E un’altra versione con `NOT EXISTS`.

```sql
SELECT p1.pnum
FROM p p1
WHERE p1.weight >= ALL
    (SELECT p2.weight
     FROM p p2
     WHERE p1.pnum <> p2.pnum);
```

```sql
SELECT p1.pnum
FROM p p1
WHERE NOT EXISTS
    (SELECT *
     FROM p p2
     WHERE p2.weight > p1.weight);
```

### Esercizio 6.2

- Trovare i nomi dei fornitori che forniscono tutte le parti (senza utilizzare operatori aggregati).

```sql
SELECT sname
FROM s
WHERE NOT EXISTS
    (SELECT *
     FROM p
     WHERE p.pnum NOT IN
        (SELECT sp.pnum
         FROM sp
         WHERE sp.snum = s.snum));
```

- Si trova prima le parti non fornite da uno specifico fornitore.

```sql
SELECT sname
FROM s
WHERE NOT EXISTS (
    (SELECT p.pnum FROM p)
     EXCEPT
    (SELECT sp.pnum FROM sp WHERE sp.snum = s.snum)
);
```

### Esercizio 6.3

- Trovare i nomi dei fornitori che forniscono almeno tutti i prodotti forniti da S2 (senza utilizzare operatori aggregati).

```sql
SELECT s.sname
FROM s
WHERE NOT EXISTS (
    SELECT *
    FROM sp sp1
    WHERE sp1.snum = 'S2' AND sp1.pnum NOT IN
        (SELECT sp2.pnum FROM sp sp2 WHERE sp2.snum = s.snum));
```

- Si trovano prima i prodotti forniti da S2 ma non da uno specifico fornitore.

```sql
SELECT s.sname
FROM s
WHERE NOT EXISTS (
    (SELECT sp1.pnum FROM sp sp1 WHERE sp1.snum = 'S2')
    EXCEPT
    (SELECT sp2.pnum FROM sp sp2 WHERE sp2.snum = s.snum));
```
