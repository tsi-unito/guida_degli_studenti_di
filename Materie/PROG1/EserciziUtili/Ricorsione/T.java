import java.util.Arrays;

/** Scrivere un metodo t con le seguenti caratteristiche:
  - t ha un primo parametro formale a di tipo array di interi ed un secondo parametro formale m di tipo intero;
  - t restituisce un array di booleani in cui ogni elemento e' true se l'elemento che gli corrisponde in a e' strettamente  inferiore al valore in l e false altrimenti;
  - t è wrapper di un solo metodo ricorsivo co-variante che risolve effettivamente il problema.
  Per ipotesi, l'array a passato al metodo T come parametro attuale non puo' essere null.
  NOTA. Il metodo con firma boolean Arrays.equals(int[],int[]) restituisce true se i due parametri
  attuali sono array identici elemento per elemento, e false altrimenti.   */

public class T {

    // Co-variante
    private static void t(int[] a, int m, int i, boolean[] r) {
        if (i == 0) {
        } else {
            t(a, m, i-1, r);
            r[i-1] = (a[i-1] < m);
        }
    }

    // Wrapper
    public static boolean[] t(int[] a, int m) {
      boolean[] r =  new boolean[a.length];
      t(a, m, a.length, r);
      return r;
    }

  /* Metodi di supporto */
  // Restituisce true se gli array di booleani a e b sono uguali
  public static boolean uguali(boolean[] a, boolean[] b) {
    return Arrays.equals(a, b);
  }

  // Stampe di array
  public static void pABool(boolean[] a) {
    System.out.println(Arrays.toString(a));
  }
  public static void pAInt(int[] a) {
    System.out.println(Arrays.toString(a));
  }

    public static void main(String[] args) {
System.out.println(uguali(new boolean[] {}                       , t(new int[] {}       , 1)));
System.out.println(uguali(new boolean[] {true }                  , t(new int[] {0}      , 1)));
System.out.println(uguali(new boolean[] {false}                  , t(new int[] {1}      , 1)));
System.out.println(uguali(new boolean[] {true ,false}            , t(new int[] {0,1}    , 1)));
System.out.println(uguali(new boolean[] {false,true }            , t(new int[] {1,0}    , 1)));
System.out.println(uguali(new boolean[] {true ,true }            , t(new int[] {0,0}    , 1)));
System.out.println(uguali(new boolean[] {false,false}            , t(new int[] {1,1}    , 1)));
System.out.println(uguali(new boolean[] {false,false,false}      , t(new int[] {1,1,1}  , 1)));
System.out.println(uguali(new boolean[] {true ,false,true ,false}, t(new int[] {0,1,0,2}, 1)));
System.out.println(uguali(new boolean[] {false,true ,false,true }, t(new int[] {1,0,1,0}, 1)));
    }
}