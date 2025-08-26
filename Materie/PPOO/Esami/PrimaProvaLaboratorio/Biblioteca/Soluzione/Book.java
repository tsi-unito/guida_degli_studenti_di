

public class Book {
    private static int counter=1;
    private int id;
    private String Titolo;
    private int NumeroCopie=0;
    public Book(String Titolo,int NumeroCopie) {
        if(NumeroCopie<0){
            throw new IllegalArgumentException("Il numero di copie non può essere negativo, valore inserito:"+NumeroCopie);
        }
        this.id=counter;
        counter++;
        this.Titolo=Titolo;
        this.NumeroCopie=NumeroCopie;
    }
    public Book(String Titolo) {
        this.id=counter;
        counter++;
        this.Titolo=Titolo;
        this.NumeroCopie=0;
    }   
    public int getId(){
        return this.id;
    }
    public String getTitolo(){
        return Titolo;
    }
    public int getNumeroCopie(){
        return NumeroCopie;
    }
    public void setTitolo(String Titolo){
        this.Titolo=Titolo;
    }
    public boolean equals(Book libro){
        if(libro==null){
            return false;
        }
        return this.id==libro.id;
    }
    public String toString(){
        return "L'id del libro è "+this.id+"\n"+"Il titolo del libro è "+this.Titolo+"\n"+"Il numero di copie presenti è "+this.NumeroCopie+"\n";
    }
    public boolean borrow(){
        if(NumeroCopie<=0){
            return false;
            //throw new IllegalStateException("Numero di copie insufficenti");
        }
        this.NumeroCopie--;
        System.out.println("Operazioni di prestito avvenuta con successo");
        return true;
    }
    public boolean returnBook(){
        this.NumeroCopie++;
        System.out.println("Operazioni di restituzione avvenuta con successo");
        return true;
    }
    public static int getTotalBooks(){
        return counter-1;
    }
}
