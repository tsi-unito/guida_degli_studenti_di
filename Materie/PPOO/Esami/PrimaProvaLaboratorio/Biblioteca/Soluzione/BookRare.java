

public class BookRare extends Book{
    private int minCopies;
    public BookRare(String Titolo,int NumeroCopieDisponibili,int minCopies){
        super(Titolo,NumeroCopieDisponibili);
        if(NumeroCopieDisponibili < minCopies || minCopies<0){
            throw new IllegalArgumentException("Argomenti Invalidi");
        }
        this.minCopies=minCopies;
    }
    public boolean borrow(){
        if(minCopies>=this.getNumeroCopie()){
            return false;
            //throw new IllegalStateException("Operazione di prestito fallita a causa della quantita di copie disponibili");
        }
        return super.borrow();
    }
    public int getMinCopies(){
        return minCopies;
    }
}
