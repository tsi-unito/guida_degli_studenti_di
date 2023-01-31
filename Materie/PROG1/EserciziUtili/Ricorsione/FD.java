import java.util.Arrays;

/** Scrivere un metodo fD con le seguenti caratteristiche:
  - fD ha un parametro formale a di tipo array di interi;
  - fD restituisce un array che contiene tutti e soli gli interi dispari inizialmente in a, rispettando l'ordine originale degli elementi;
  - fD è wrapper di un paio di metodi ricorsivi con le seguenti caratteristiche:
      + uno contro-variante che conta quanti elementi in a dovranno essere restituiti nell'array risultato;
      + uno co-variante che risolve effettivamente il problema.
  Per ipotesi, l'array a passato al metodo fD come parametro attuale non puo' essere null. 
  NOTA. Il metodo con firma boolean Arrays.equals(int[],int[]) restituisce true se i due parametri
  attuali sono array identici elemento per elemento, e false altrimenti.*/
  
public class FD {

    // Contro-variante
    public static int contaDispari(int[] a, int i) {
        if (i == a.length)
          return 0;
        else if (a[i]%2 == 1) 
                return 1 + contaDispari(a, i+1);
             else
                return 0 + contaDispari(a, i+1);
    }

    // Co-variante
    public static int fD(int[] a, int i, int[] r) {
        if (i == 0)
          return 0;
        else {
          int indiceR = fD(a, i-1, r);
          if (a[i-1]%2 == 1) {
             r[indiceR] = a[i-1];
             return indiceR+1;
          } else 
             return indiceR;
        }
    }

    // Wrapper
    public static int[] fD(int[] a) {
      int[] r = new int[contaDispari(a, 0)];
      fD(a, a.length, r); 
      return r;
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
System.out.println(uguali(new int[] {}     , fD(new int[] {}       )));
System.out.println(uguali(new int[] {}     , fD(new int[] {0}      )));
System.out.println(uguali(new int[] {1}    , fD(new int[] {1}      )));
System.out.println(uguali(new int[] {1}    , fD(new int[] {0,1}    )));
System.out.println(uguali(new int[] {1}    , fD(new int[] {1,0}    )));
System.out.println(uguali(new int[] {}     , fD(new int[] {0,0,0}  )));
System.out.println(uguali(new int[] {1}    , fD(new int[] {0,1,0}  )));
System.out.println(uguali(new int[] {1,1,1}, fD(new int[] {1,1,1}  )));
System.out.println(uguali(new int[] {1,1}  , fD(new int[] {0,1,0,1})));
System.out.println(uguali(new int[] {1,1}  , fD(new int[] {1,0,1,0})));
    }
}