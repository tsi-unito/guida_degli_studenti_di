import java.util.Arrays;

/** Scrivere un metodo fPosDValD con le seguenti caratteristiche:
  - fPosDValD ha un parametro formale a di tipo array di interi;
  - fPosDValD restituisce un array che contiene il doppio di tutti e soli gli interi di posizione dispari inizialmente in a, rispettando l'ordine originale degli elementi;
  - fPosDValD è wrapper un metodo metodo ricorsivo dicotomico che risolve effettivamente il problema.
  Per ipotesi, l'array a passato al metodo fPosDValD come parametro attuale deve avere almeno un elemento.
  NOTA. Il metodo con firma boolean Arrays.equals(int[],int[]) restituisce true se i due parametri
  attuali sono array identici elemento per elemento, e false altrimenti.*/

public class FPosDValDDic {

    // Dicotomico
    public static void fPosDValD(int[] a, int l, int r, int[] ris) {
      if(r-l == 0) {
      } else {
          if (r-l == 1) {
            if (l%2 == 1) {
               ris[l/2] = 2*a[l];
            } 
          } else {
            int m = (r+l)/2;
            fPosDValD(a, l, m, ris);
            fPosDValD(a, m, r, ris);
          }
      }
    }

// Wrapper
    public static int[] fPosDValD(int[] a) {
      int[] ris = new int[a.length/2];
      fPosDValD(a, 0, a.length, ris); 
      return ris;
    }

    
  /* Metodi di supporto */
  // Restituisce true se gli array di interi a e b sono uguali
  public static boolean uguali(int[] a, int[] b) {
    return Arrays.equals(a, b);
  }

  // Stampa un array di interi 
  public static void pA(int[] a) {
    System.out.println(Arrays.toString(a));
  }    

    public static void main(String[] args) {
System.out.println(uguali(new int[] {}   , fPosDValD(new int[] {}      )));
System.out.println(uguali(new int[] {}   , fPosDValD(new int[] {0}      )));
System.out.println(uguali(new int[] {}   , fPosDValD(new int[] {1}      )));
System.out.println(uguali(new int[] {2}  , fPosDValD(new int[] {0,1}    )));
System.out.println(uguali(new int[] {0}  , fPosDValD(new int[] {1,0}    )));
System.out.println(uguali(new int[] {0}  , fPosDValD(new int[] {0,0,0}  )));
System.out.println(uguali(new int[] {2}  , fPosDValD(new int[] {0,1,0}  )));
System.out.println(uguali(new int[] {2}  , fPosDValD(new int[] {1,1,1}  )));
System.out.println(uguali(new int[] {2,4}, fPosDValD(new int[] {0,1,0,2})));
System.out.println(uguali(new int[] {0,0}, fPosDValD(new int[] {1,0,1,0})));
    }
}