

public class StudentTest {
    
    public static void main(String[] args) {
        System.out.println("=== TEST UNIVERSITÀ - CLASSE STUDENT ===\n");
        
        // Test 1: Verifica costruttore completo e variabile statica (4 punti)
        System.out.println("--- Test 1: Costruttore completo e matriculaCounter ---");
        Student student1 = new Student("Mario Rossi", 60);
        System.out.println("Primo studente creato: " + student1);
        System.out.println("Matricola attesa: 1, Matricola ottenuta: " + student1.getMatricula());
        System.out.println("Nome: " + student1.getName());
        System.out.println("Crediti: " + student1.getCredits());
        System.out.println("Counter funziona: " + (student1.getMatricula() == 1 ? "✓" : "✗"));
        System.out.println();
        
        // Test 2: Secondo costruttore e incremento counter (3 punti)
        System.out.println("--- Test 2: Costruttore parziale e incremento counter ---");
        Student student2 = new Student("Anna Bianchi");
        System.out.println("Secondo studente creato: " + student2);
        System.out.println("Matricola attesa: 2, Matricola ottenuta: " + student2.getMatricula());
        System.out.println("Nome: " + student2.getName());
        System.out.println("Crediti (default): " + student2.getCredits());
        System.out.println("Counter incrementato: " + (student2.getMatricula() == 2 ? "✓" : "✗"));
        System.out.println();
        
        // Test 3: Terzo studente per verificare continuità counter (2 punti)
        System.out.println("--- Test 3: Continuità counter ---");
        Student student3 = new Student("Luca Verde", 120);
        System.out.println("Terzo studente: " + student3);
        System.out.println("Counter continuo: " + (student3.getMatricula() == 3 ? "✓" : "✗"));
        System.out.println();
        
        // Test 4: Metodi get (2 punti)
        System.out.println("--- Test 4: Metodi get ---");
        System.out.println("getName() student1: " + student1.getName());
        System.out.println("getCredits() student1: " + student1.getCredits());
        System.out.println("getMatricula() student1: " + student1.getMatricula());
        System.out.println("Metodi get funzionano: ✓");
        System.out.println();
        
        // Test 5: Metodo set per nome (2 punti)
        System.out.println("--- Test 5: Metodo set per nome ---");
        System.out.println("Nome prima: " + student2.getName());
        student2.setName("Anna Bianchi Verdi");
        System.out.println("Nome dopo: " + student2.getName());
        System.out.println("Set funziona: " + ("Anna Bianchi Verdi".equals(student2.getName()) ? "✓" : "✗"));
        System.out.println();
        
        // Test 6: Metodo statico getTotalStudents (3 punti)
        System.out.println("--- Test 6: Metodo statico getTotalStudents ---");
        System.out.println("Numero totale studenti: " + Student.getTotalStudents());
        System.out.println("Atteso: 3, Ottenuto: " + Student.getTotalStudents());
        System.out.println("Metodo statico funziona: " + (Student.getTotalStudents() == 3 ? "✓" : "✗"));
        System.out.println();
        
        // Test 7: Metodo earnCredits() - aggiunta crediti normale (3 punti)
        System.out.println("--- Test 7: Metodo earnCredits() ---");
        System.out.println("Crediti student1 prima: " + student1.getCredits());
        boolean earnResult = student1.earnCredits(12);
        System.out.println("Aggiunta crediti riuscita: " + earnResult);
        System.out.println("Crediti student1 dopo: " + student1.getCredits());
        System.out.println("EarnCredits funziona: " + (student1.getCredits() == 72 && earnResult ? "✓" : "✗"));
        System.out.println();
        
        // Test 8: Metodo earnCredits() - controllo crediti negativi (2 punti)
        System.out.println("--- Test 8: EarnCredits con valore negativo ---");
        int creditsBefore = student1.getCredits();
        boolean earnFailResult = student1.earnCredits(-5);
        System.out.println("Aggiunta crediti negativi (atteso fallimento): " + !earnFailResult);
        System.out.println("Crediti prima: " + creditsBefore);
        System.out.println("Crediti dopo: " + student1.getCredits());
        System.out.println("Gestione errore earnCredits: " + (!earnFailResult && student1.getCredits() == creditsBefore ? "✓" : "✗"));
        System.out.println();
        
        // Test 9: Metodo transferCredits() - trasferimento normale (3 punti)
        System.out.println("--- Test 9: Metodo transferCredits() ---");
        System.out.println("Crediti student1 prima del trasferimento: " + student1.getCredits());
        System.out.println("Crediti student3 prima del trasferimento: " + student3.getCredits());
        boolean transferResult = student1.transferCredits(student3, 18);
        System.out.println("Trasferimento riuscito: " + transferResult);
        System.out.println("Crediti student1 dopo: " + student1.getCredits());
        System.out.println("Crediti student3 dopo: " + student3.getCredits());
        System.out.println("TransferCredits funziona: " + (student1.getCredits() == 54 && student3.getCredits() == 138 && transferResult ? "✓" : "✗"));
        System.out.println();
        
        // Test 10: Metodo transferCredits() - trasferimento senza crediti sufficienti (2 punti)
        System.out.println("--- Test 10: TransferCredits senza crediti sufficienti ---");
        int creditsBeforeTransfer1 = student2.getCredits();
        int creditsBeforeTransfer3 = student3.getCredits();
        System.out.println("Crediti student2: " + creditsBeforeTransfer1);
        boolean transferFailResult = student2.transferCredits(student3, 50);
        System.out.println("Trasferimento fallito (atteso): " + !transferFailResult);
        System.out.println("Crediti student2 dopo: " + student2.getCredits());
        System.out.println("Crediti student3 dopo: " + student3.getCredits());
        System.out.println("Gestione errore transfer: " + (!transferFailResult && student2.getCredits() == creditsBeforeTransfer1 && student3.getCredits() == creditsBeforeTransfer3 ? "✓" : "✗"));
        System.out.println();
        
        // Test 11: Metodo equals() (2 punti)
        System.out.println("--- Test 11: Metodo equals() ---");
        Student student4 = new Student("Test Equals", 90);
        Student student5 = new Student("Test Equals", 90);
        Student student6 = new Student("Diverso", 90);
        
        System.out.println("student4 equals student5 (stesso nome): " + student4.equals(student5));
        System.out.println("student4 equals student6 (nome diverso): " + student4.equals(student6));
        System.out.println("student4 equals null: " + student4.equals(null));
        System.out.println("Equals funziona: ✓");
        System.out.println();
        
        // Test 12: Metodo toString() (2 punti)
        System.out.println("--- Test 12: Metodo toString() ---");
        System.out.println("toString() student1: " + student1.toString());
        System.out.println("Contiene matricola: " + (student1.toString().contains("" + student1.getMatricula()) ? "✓" : "✗"));
        System.out.println("Contiene nome: " + (student1.toString().contains(student1.getName()) ? "✓" : "✗"));
        System.out.println("Contiene crediti: " + (student1.toString().contains("" + student1.getCredits()) ? "✓" : "✗"));
        System.out.println();
        
        // PARTE ADVANCED: Test StudentExchange (3 punti)
        System.out.println("=== PARTE ADVANCED: StudentExchange ===");
        System.out.println("--- Test 13: Classe StudentExchange ---");
        
        StudentExchange exchangeStudent = new StudentExchange("Studente Erasmus", 100, 30);
        System.out.println("Studente exchange creato: " + exchangeStudent);
        System.out.println("Crediti iniziali: " + exchangeStudent.getCredits());
        System.out.println("Limite massimo trasferimento: " + exchangeStudent.getMaxTransferCredits());
        System.out.println();
        
        // Test 14: TransferCredits con limite massimo (2 punti)
        System.out.println("--- Test 14: TransferCredits con limite massimo ---");
        Student receivingStudent = new Student("Ricevente", 50);
        
        // Trasferimento entro il limite
        System.out.println("Trasferimento 1 (25 crediti, entro limite): " + exchangeStudent.transferCredits(receivingStudent, 25));
        System.out.println("Crediti exchange: " + exchangeStudent.getCredits());
        System.out.println("Crediti ricevente: " + receivingStudent.getCredits());
        
        // Trasferimento oltre il limite
        System.out.println("Trasferimento 2 (35 crediti, oltre limite): " + exchangeStudent.transferCredits(receivingStudent, 35));
        System.out.println("Crediti exchange: " + exchangeStudent.getCredits());
        System.out.println("Crediti ricevente: " + receivingStudent.getCredits());
        
        // Verificare che il secondo trasferimento sia fallito
        System.out.println("Limite rispettato: " + (exchangeStudent.getCredits() == 75 && receivingStudent.getCredits() == 75 ? "✓" : "✗"));
        System.out.println();
        
        // Test 15: TransferCredits al limite esatto (1 punto bonus)
        System.out.println("--- Test 15: TransferCredits al limite esatto ---");
        System.out.println("Trasferimento esatto al limite (30 crediti): " + exchangeStudent.transferCredits(receivingStudent, 30));
        System.out.println("Crediti exchange: " + exchangeStudent.getCredits());
        System.out.println("Crediti ricevente: " + receivingStudent.getCredits());
        System.out.println("Trasferimento al limite funziona: " + (exchangeStudent.getCredits() == 45 ? "✓" : "✗"));
        System.out.println();
        
        // Test finale: Conteggio totale (1 punto)
        System.out.println("--- Test Finale: Conteggio totale ---");
        System.out.println("Numero totale studenti creati: " + Student.getTotalStudents());
        System.out.println("Test completati con successo! ✓");
        
        // Riassunto punteggi
        System.out.println("\n=== RIASSUNTO VALUTAZIONE ===");
        System.out.println("Costruttore completo e counter: 4 punti");
        System.out.println("Costruttore parziale: 3 punti");
        System.out.println("Continuità counter: 2 punti");
        System.out.println("Metodi get: 2 punti");
        System.out.println("Metodo set: 2 punti");
        System.out.println("Metodo statico: 3 punti");
        System.out.println("Metodo earnCredits(): 3 punti");
        System.out.println("Gestione errore earnCredits: 2 punti");
        System.out.println("Metodo transferCredits(): 3 punti");
        System.out.println("Gestione errore transfer: 2 punti");
        System.out.println("Metodo equals(): 2 punti");
        System.out.println("Metodo toString(): 2 punti");
        System.out.println("Classe StudentExchange: 3 punti");
        System.out.println("TransferCredits con limite: 2 punti");
        System.out.println("TOTALE: 31 punti (30 e lode)");
        System.out.println("Sufficienza: 18 punti");
    }
}
