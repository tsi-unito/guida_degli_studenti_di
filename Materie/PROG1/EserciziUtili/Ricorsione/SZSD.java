import java.util.Arrays;

/** Scrivere un metodo sZSD con le seguenti caratteristiche:
  - sZSD ha un parametro formale a di tipo array di interi;
  - sZSD è wrapper di un metodo ricorsivo dicotomico che, tramite il meccanismo di aliasing, 
  modifica a come segue:
    + ogni valore che si trova nella posizione immediatamente successiva ad una cella con valore pari
    è sostituito con 0, ammesso che tale cella successiva esista;
  - sZSD restituisce una copia di a, per mezzo dell'assegnamento int[] b = Array.copyOf(a, a.length).
  
  Per ipotesi, il parametro attuale a, passato al metodo sZSD, può essere null o può contenere 0 elementi.
  NOTA. Il metodo con firma boolean Arrays.equals(int[],int[]) restituisce true se i due parametri
  attuali sono array identici elemento per elemento, e false altrimenti. Il metodo con firma boolean 
  Arrays.toString(int[]) restituisce una stringa con gli elementi 
  del parametro formale. */

public class SZSD {
  // Dicotomico
  static void sZSD(int[] a, int l, int r) {
    if (l < r) { // almeno un elemento in [l,r)
      if (1 == r-l) { // un elemento in [l,r)
        if (a[l]%2 == 0) { // elemento multiplo di n
          if (l+1 < a.length) { // elemento successivo esiste
            a[l+1] = 0;
          }
        }
      } else { // più di un elemento in [l,r)
         int m = (r+l)/2;
         sZSD(a, l, m);
         sZSD(a, m, r);
      } 
    }
  }
  
  // Wrapper
  static int[] sZSD(int[] a) {
    int[] ris = null;
    if (a != null) {
      sZSD(a, 0, a.length);
      ris = new int[a.length];
      ris = Arrays.copyOf(a, a.length);
    }
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
System.out.println(uguali(null               , sZSD(null               )));
System.out.println(uguali(new int[] {}       , sZSD(new int[] {}       )));
System.out.println(uguali(new int[] {1}      , sZSD(new int[] {1}      )));
System.out.println(uguali(new int[] {2}      , sZSD(new int[] {2}      )));
System.out.println(uguali(new int[] {1,2}    , sZSD(new int[] {1,2}    )));
System.out.println(uguali(new int[] {2,0}    , sZSD(new int[] {2,1}    )));
System.out.println(uguali(new int[] {2,0,0}  , sZSD(new int[] {2,1,4}  )));
System.out.println(uguali(new int[] {1,1,1}  , sZSD(new int[] {1,1,1}  )));
System.out.println(uguali(new int[] {1,2,0,0}, sZSD(new int[] {1,2,1,4})));
System.out.println(uguali(new int[] {2,0,0,0}, sZSD(new int[] {2,1,4,1})));
    }
}