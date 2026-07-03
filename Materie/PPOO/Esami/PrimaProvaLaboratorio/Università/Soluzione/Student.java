
public class Student {
    private static int matriculaCounter=0;
    private int matricula;
    private String name;
    private int credits=0;;
    public Student(String name,int credits){
        if(credits<0){
            throw new IllegalArgumentException("Numero crediti Negativo");
        }
        this.name=name;
        this.credits=credits;
        matriculaCounter++;
        this.matricula=matriculaCounter;
    }
    public Student(String name){
        this.name=name;
        matriculaCounter++;
        this.matricula=matriculaCounter;
    }
    public String getName(){
        return name;
    }
    public int getCredits(){
        return credits;
    }
    public int getMatricula(){
        return matricula;
    }
    public void setName(String name){
        this.name=name;
    }
    public boolean equals(Student studente){
        if(studente==null){
            return false;
        }
        return this.matricula==studente.matricula;
    }
    public String toString(){
        return "Numero di matricola: "+matricula+"\n"+"Nome: "+name+"\n"+"Numero di crediti: "+credits+"\n";
    }
    public boolean earnCredits(int credits) {
        if(credits<0){
            return false;
            //throw new IllegalArgumentException("Numero di crediti negativo")
        }
        this.credits+=credits;
        return true;
    }
    public boolean transferCredits(Student studente,int credits){
        if(credits<0 || credits>this.credits || studente==null){
            return false;
            //throw new IllegalArgumentException("Argomento invalido")
        }
        this.credits-=credits;
        studente.credits+=credits;
        return true;
    }
    public static int getTotalStudents(){
        return matriculaCounter;
    }
}