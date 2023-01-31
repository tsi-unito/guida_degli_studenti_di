import java.util.Arrays;

/** Scrivere un metodo fMag con le seguenti caratteristiche:
  - fMag ha un primo parametro formale a di tipo array di interi ed un secondo parametro formale m di tipo intero;
  - fMag restituisce un array che contiene tutti e soli gli interi inizialmente in a e strettamente maggiori del valore in m, rispettando l'ordine originale degli elementi;
  - fMag *non* puo' contare il numero di elementi in a che soddisfano la proprieta' indicata per dimensionare opportunamente l'array da produrre come risultato. La conseguenza è che occorrerà scrivere anche un metodo append (ricorsivo o iterativo) che, dati due array x, ed y di interi, ne costruisce un terzo z la cui prima parte contiene tutti e soli i valori in x e la seconda tutti e soli i valori di y;
  - fMag è wrapper di un solo metodo ricorsivo dicotomico che risolve effettivamente il problema.
  Per ipotesi, l'array a passato al metodo fD come parametro attuale non puo' essere null.
  NOTA. Il metodo con firma boolean Arrays.equals(int[],int[]) restituisce true se i due parametri
  attuali sono array identici elemento per elemento, e false altrimenti.   */

public class FMagDic {

    // Dicotomica
    private static int[] fMag(int[] a, int m, int l, int r) {
        if (r-l == 0) // intervallo vuoto, con 0 elementi
          return new int[0];
        else
          if (r-l == 1) // intervallo con un elemento
            if (a[l] > m) { // l'elemento soddisfa la condizione
               int[] e = new int[1];
               e[0] = a[l];
               return e;
            } else // l'elemento non soddisfa la condizione
               return new int[0];
          else { // intervallo con più di un elemento tra cui individuare quelli di interesse
            int mezzo = (r+l)/2;
            int[] sx = fMag(a, m, l, mezzo);
            int[] dx = fMag(a, m, mezzo, r);
            return append(sx, dx);
          }
    }

    // Wrapper
    public static int[] fMag(int[] a, int m) {
        return fMag(a, m, 0, a.length);
    }

    // Appende array y ad array x
    public static int[] append(int[] x, int[] y) {
        int[] z = new int[x.length+y.length];
        int i;
        for(i = 0; i < x.length; i++)
            z[i] = x[i];
        for(i = 0; i < y.length; i++)
            z[x.length+i] = y[i];
        return z;
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