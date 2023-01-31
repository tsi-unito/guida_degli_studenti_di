import java.util.Arrays;

/** Scrivere un metodo fMag con le seguenti caratteristiche:
  - fMag ha un primo parametro formale a di tipo array di interi ed un secondo parametro formale l di tipo intero;
  - fMag restituisce un array che contiene tutti e soli gli interi inizialmente in a e strettamente maggiori del valore in l, rispettando l'ordine originale degli elementi;
  - fMag *non* puo' contare il numero di elementi in a che soddisfano la proprieta' indicata per dimensionare opportunamente l'array da produrre come risultato;
  - fMag è wrapper di un solo metodo ricorsivo co-variante che risolve effettivamente il problema.
  Per ipotesi, l'array a passato al metodo fD come parametro attuale non puo' essere null. 
  NOTA. Il metodo con firma boolean Arrays.equals(int[],int[]) restituisce true se i due parametri
  attuali sono array identici elemento per elemento, e false altrimenti.   */
  
public class FMag {

    // Co-variante
    private static int[] fMag(int[] a, int l, int i) {
        if (i == 0) {
            return new int[0];
        } else { 
            int[] tmp = fMag(a, l, i-1);
            if (a[i-1] > l) { 
                int[] r = new int[tmp.length + 1]; 
                copiaDi(tmp, r, 0);
                r[r.length-1] = a[i-1];
                return r;
            } else 
                return tmp;
        }
    }

    // Wrapper
    public static int[] fMag(int[] a, int l) {
        return fMag(a, l, a.length);
    }

    /* Metodo contro-variante che copia tmp in r, assumendo 
    che tmp.length<=r.length. */
    private static void copiaDi(int[] tmp, int[] r, int i) {
        if (i == tmp.length) {
        } else { 
            r[i] = tmp[i];
            copiaDi(tmp, r, i+1);
        }
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
System.out.println(uguali(new int[] {}     , fMag(new int[] {}       , 0)));
System.out.println(uguali(new int[] {}     , fMag(new int[] {0}      , 0)));
System.out.println(uguali(new int[] {1}    , fMag(new int[] {1}      , 0)));
System.out.println(uguali(new int[] {1}    , fMag(new int[] {0,1}    , 0)));
System.out.println(uguali(new int[] {1}    , fMag(new int[] {1,0}    , 0)));
System.out.println(uguali(new int[] {}     , fMag(new int[] {0,0,0}  , 0)));
System.out.println(uguali(new int[] {1}    , fMag(new int[] {0,1,0}  , 0)));
System.out.println(uguali(new int[] {1,1,1}, fMag(new int[] {1,1,1}  , 0)));
System.out.println(uguali(new int[] {1,2}  , fMag(new int[] {0,1,0,2}, 0)));
System.out.println(uguali(new int[] {1,1}  , fMag(new int[] {1,0,1,0}, 0)));
    }
}