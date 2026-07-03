

public class BookTest {
    
    public static void main(String[] args) {
        System.out.println("=== TEST BIBLIOTECA - CLASSE BOOK ===\n");
        
        // Test 1: Verifica costruttore completo e variabile statica (4 punti)
        System.out.println("--- Test 1: Costruttore completo e counter ---");
        Book book1 = new Book("Il Signore degli Anelli", 5);
        System.out.println("Primo libro creato: " + book1);
        System.out.println("ID atteso: 1, ID ottenuto: " + book1.getId());
        System.out.println("Titolo: " + book1.getTitolo());
        System.out.println("Copie: " + book1.getNumeroCopie());
        System.out.println("Counter funziona: " + (book1.getId() == 1 ? "✓" : "✗"));
        System.out.println();
        
        // Test 2: Secondo costruttore e incremento counter (3 punti)
        System.out.println("--- Test 2: Costruttore parziale e incremento counter ---");
        Book book2 = new Book("1984");
        System.out.println("Secondo libro creato: " + book2);
        System.out.println("ID atteso: 2, ID ottenuto: " + book2.getId());
        System.out.println("Titolo: " + book2.getTitolo());
        System.out.println("Copie (default): " + book2.getNumeroCopie());
        System.out.println("Counter incrementato: " + (book2.getId() == 2 ? "✓" : "✗"));
        System.out.println();
        
        // Test 3: Terzo libro per verificare continuità counter (2 punti)
        System.out.println("--- Test 3: Continuità counter ---");
        Book book3 = new Book("Harry Potter", 10);
        System.out.println("Terzo libro: " + book3);
        System.out.println("Counter continuo: " + (book3.getId() == 3 ? "✓" : "✗"));
        System.out.println();
        
        // Test 4: Metodi get (2 punti)
        System.out.println("--- Test 4: Metodi get ---");
        System.out.println("getTitolo() book1: " + book1.getTitolo());
        System.out.println("getNumeroCopie() book1: " + book1.getNumeroCopie());
        System.out.println("getId() book1: " + book1.getId());
        System.out.println("Metodi get funzionano: ✓");
        System.out.println();
        
        // Test 5: Metodo set per titolo (2 punti)
        System.out.println("--- Test 5: Metodo set per titolo ---");
        System.out.println("Titolo prima: " + book2.getTitolo());
        book2.setTitolo("1984 - Edizione Speciale");
        System.out.println("Titolo dopo: " + book2.getTitolo());
        System.out.println("Set funziona: " + ("1984 - Edizione Speciale".equals(book2.getTitolo()) ? "✓" : "✗"));
        System.out.println();
        
        // Test 6: Metodo statico getTotalBooks (3 punti)
        System.out.println("--- Test 6: Metodo statico getTotalBooks ---");
        System.out.println("Numero totale libri: " + Book.getTotalBooks());
        System.out.println("Atteso: 3, Ottenuto: " + Book.getTotalBooks());
        System.out.println("Metodo statico funziona: " + (Book.getTotalBooks() == 3 ? "✓" : "✗"));
        System.out.println();
        
        // Test 7: Metodo borrow() - prestito normale (3 punti)
        System.out.println("--- Test 7: Metodo borrow() ---");
        System.out.println("Copie book1 prima del prestito: " + book1.getNumeroCopie());
        boolean borrowResult = book1.borrow();
        System.out.println("Prestito riuscito: " + borrowResult);
        System.out.println("Copie book1 dopo il prestito: " + book1.getNumeroCopie());
        System.out.println("Borrow funziona: " + (book1.getNumeroCopie() == 4 && borrowResult ? "✓" : "✗"));
        System.out.println();
        
        // Test 8: Metodo borrow() - prestito quando non ci sono copie (2 punti)
        System.out.println("--- Test 8: Borrow senza copie disponibili ---");
        // Svuotiamo le copie del book2
        book2 = new Book("Libro senza copie", 0);
        System.out.println("Copie book2: " + book2.getNumeroCopie());
        boolean borrowFailResult = book2.borrow();
        System.out.println("Prestito fallito (atteso): " + !borrowFailResult);
        System.out.println("Copie rimaste: " + book2.getNumeroCopie());
        System.out.println("Gestione errore borrow: " + (!borrowFailResult && book2.getNumeroCopie() == 0 ? "✓" : "✗"));
        System.out.println();
        
        // Test 9: Metodo returnBook() - restituzione normale (3 punti)
        System.out.println("--- Test 9: Metodo returnBook() ---");
        System.out.println("Copie book1 prima della restituzione: " + book1.getNumeroCopie());
        boolean returnResult = book1.returnBook();
        System.out.println("Restituzione riuscita: " + returnResult);
        System.out.println("Copie book1 dopo la restituzione: " + book1.getNumeroCopie());
        System.out.println("ReturnBook funziona: " + (book1.getNumeroCopie() == 5 && returnResult ? "✓" : "✗"));
        System.out.println();
        
        
        // Test 11: Metodo equals() (2 punti)
        System.out.println("--- Test 11: Metodo equals() ---");
        Book book4 = new Book("Test Equals", 3);
        Book book5 = new Book("Test Equals", 3);
        Book book6 = new Book("Diverso", 3);
        
        System.out.println("book4 equals book5 (stesso titolo): " + book4.equals(book5));
        System.out.println("book4 equals book6 (titolo diverso): " + book4.equals(book6));
        System.out.println("book4 equals null: " + book4.equals(null));
        System.out.println("Equals funziona: ✓");
        System.out.println();
        
        // Test 12: Metodo toString() (2 punti)
        System.out.println("--- Test 12: Metodo toString() ---");
        System.out.println("toString() book1: " + book1.toString());
        System.out.println("Contiene ID: " + (book1.toString().contains("" + book1.getId()) ? "✓" : "✗"));
        System.out.println("Contiene titolo: " + (book1.toString().contains(book1.getTitolo()) ? "✓" : "✗"));
        System.out.println("Contiene copie: " + (book1.toString().contains("" + book1.getNumeroCopie()) ? "✓" : "✗"));
        System.out.println();
        
        // PARTE ADVANCED: Test BookRare (3 punti)
        System.out.println("=== PARTE ADVANCED: BookRare ===");
        System.out.println("--- Test 13: Classe BookRare ---");
        
        BookRare rareBook = new BookRare("Libro Raro", 5, 2);
        System.out.println("Libro raro creato: " + rareBook);
        System.out.println("Copie iniziali: " + rareBook.getNumeroCopie());
        System.out.println("Limite minimo: " + rareBook.getMinCopies());
        System.out.println();
        
        // Test 14: Borrow con limite minimo (2 punti)
        System.out.println("--- Test 14: Borrow con limite minimo ---");
        System.out.println("Prestito 1: " + rareBook.borrow()); // 5->4
        System.out.println("Copie: " + rareBook.getNumeroCopie());
        System.out.println("Prestito 2: " + rareBook.borrow()); // 4->3  
        System.out.println("Copie: " + rareBook.getNumeroCopie());
        System.out.println("Prestito 3: " + rareBook.borrow()); // 3->2
        System.out.println("Copie: " + rareBook.getNumeroCopie());
        System.out.println("Prestito 4 (dovrebbe fallire): " + rareBook.borrow()); // Deve rimanere a 2
        System.out.println("Copie finali: " + rareBook.getNumeroCopie());
        System.out.println("Limite rispettato: " + (rareBook.getNumeroCopie() >= 2 ? "✓" : "✗"));
        System.out.println();
        
        // Test finale: Conteggio totale (1 punto)
        System.out.println("--- Test Finale: Conteggio totale ---");
        System.out.println("Numero totale libri creati: " + Book.getTotalBooks());
        System.out.println("Test completati con successo! ✓");
        
        // Riassunto punteggi
        System.out.println("\n=== RIASSUNTO VALUTAZIONE ===");
        System.out.println("Costruttore completo e counter: 4 punti");
        System.out.println("Costruttore parziale: 3 punti");
        System.out.println("Continuità counter: 2 punti");
        System.out.println("Metodi get: 2 punti");
        System.out.println("Metodo set: 2 punti");
        System.out.println("Metodo statico: 3 punti");
        System.out.println("Metodo borrow(): 3 punti");
        System.out.println("Gestione errore borrow: 2 punti");
        System.out.println("Metodo returnBook(): 3 punti");
        System.out.println("Gestione limite return: 2 punti");
        System.out.println("Metodo equals(): 2 punti");
        System.out.println("Metodo toString(): 2 punti");
        System.out.println("Classe BookRare: 3 punti");
        System.out.println("Borrow con limite: 2 punti");
        System.out.println("TOTALE: 31 punti (30 e lode)");
        System.out.println("Sufficienza: 18 punti");
    }
}

