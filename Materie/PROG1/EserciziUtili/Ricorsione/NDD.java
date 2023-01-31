import java.util.Arrays;

/** Scrivere un metodo nDD con le seguenti caratteristiche:
  - nDD ha un parametro formale a di tipo array di interi ed un intero n;
  - nDD restituisce un nuovo array, rispetto ad a, che contiene tutti e soli i multipli di n 
  che si trovano in a, rispettando l'ordine originale degli elementi;
  - nDD è wrapper di un metodo con le seguenti caratteristiche:
    + è ricorsivo dicotomico;
    + risolve effettivamente il problema usando il metodo append, fornito a corredo del testo.
  Per ipotesi, il parametro attuale a, passato al metodo nDD, può essere null o può contenere 0 elementi.
  NOTA. Il metodo con firma boolean Arrays.equals(int[],int[]) restituisce true se i due parametri
  attuali sono array identici elemento per elemento, e false altrimenti.
  Il metodo con firma boolean Arrays.toString(int[]) restituisce una stringa con gli elementi 
  del parametro formale. */

public class NDD {
  // Dicotomico
  static int[] nDD(int[] a, int n, int l, int r) {
    if (l < r) { // almeno un elemento in [l,r)
      if (1 == r-l) { // un elemento in [l,r)
        if (a[l]%n == 0) { // elemento multiplo di n
           int[] ris = new int[1];
           ris[0] = a[l];
           return ris;
        } else {
           return new int[] {};  
        }
      } else { // più di un elemento in [l,r)
         int m = (r+l)/2;
         int[] sx = nDD(a, n, l, m);
         int[] dx = nDD(a, n, m, r);
         return append(sx, dx);
      } 
    } else { // no elementi in [l,r)
       return new int[] {};  
    }
  }
  
  // Wrapper
  static int[] nDD(int[] a, int n) {
    int[] ris = null;
    if (a != null) {
      ris = nDD(a, n, 0, a.length);
    }
    return ris;
  }
  
  
  /* Metodi di supporto */
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

  // Restituisce true se gli array di interi a e b sono uguali
  public static boolean uguali(int[] a, int[] b) {
    return Arrays.equals(a, b);
  }

  // Stampa un array di interi
  public static void pA(int[] a) {
    System.out.println(Arrays.toString(a));
  }

  public static void main(String[] args) {
System.out.println(uguali(null           , nDD(null               , 0)));
System.out.println(uguali(new int[] {}   , nDD(new int[] {}       , 0)));
System.out.println(uguali(new int[] {}   , nDD(new int[] {1}      , 2)));
System.out.println(uguali(new int[] {2}  , nDD(new int[] {2}      , 2)));
System.out.println(uguali(new int[] {2}  , nDD(new int[] {1,2}    , 2)));
System.out.println(uguali(new int[] {2}  , nDD(new int[] {2,1}    , 2)));
System.out.println(uguali(new int[] {2,4}, nDD(new int[] {2,1,4}  , 2)));
System.out.println(uguali(new int[] {}   , nDD(new int[] {1,1,1}  , 2)));
System.out.println(uguali(new int[] {2,4}, nDD(new int[] {1,2,1,4}, 2)));
System.out.println(uguali(new int[] {2,4}, nDD(new int[] {2,1,4,1}, 2)));
    }
}