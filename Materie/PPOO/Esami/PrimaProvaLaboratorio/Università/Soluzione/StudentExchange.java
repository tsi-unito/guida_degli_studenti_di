
public class StudentExchange extends Student {
    private int maxTransferCredits;
    public StudentExchange(String name,int credits,int maxTransferCredits){
        super(name,credits);
        if(maxTransferCredits<0){
            throw new IllegalArgumentException("Argomento non valido");
        }
        this.maxTransferCredits=maxTransferCredits;
    }
    public boolean transferCredits(Student studente,int credits){
        if(credits>maxTransferCredits){
            return false;
        }
        return super.transferCredits(studente,credits);
    }
    public int getMaxTransferCredits() {
        return maxTransferCredits;
    }
    public String toString() {
        return super.toString() + "Limite trasferimento: "+maxTransferCredits;
    }
}
