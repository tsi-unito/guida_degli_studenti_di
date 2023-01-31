import java.util.Arrays;

/** Scrivere un metodo fMin con le seguenti caratteristiche:
  - fMin ha un primo parametro formale a di tipo array di interi ed un secondo parametro formale m di tipo intero;
  - fMin restituisce un array che contiene tutti e soli gli interi inizialmente in a e strettamente minori del valore in m, rispettando l'ordine originale degli elementi;
  - fMin è wrapper di un paio di metodi con le seguenti caratteristiche:
      + il primo è ricorsivo dicotomico conta quanti elementi in a dovranno essere restituiti nell'array risultato;
      + il secondo è ricorsivo co-variante e risolve effettivamente il problema.
  Per ipotesi, l'array a passato al metodo fMin come parametro attuale non puo' essere null. 
  NOTA. Il metodo con firma boolean Arrays.equals(int[],int[]) restituisce true se i due parametri
  attuali sono array identici elemento per elemento, e false altrimenti.   */

public class FMinDic {

    // Dicotomico
    public static int contaMinori(int[] a, int m, int l, int r) {
        if (r-l == 0)
          return 0;
        else 
          if (r-l == 1) 
            if (a[l] < m) 
              return 1;
            else
              return 0;
          else {
            int mezzo = (r+l)/2;
            return contaMinori(a, m, l, mezzo) + contaMinori(a, m, mezzo, r);
          }
    }

    // Co-variante
    public static int fMin(int[] a, int m, int i, int[] r) {
        if (i == 0)
          return 0;
        else {
          int indiceR = fMin(a, m, i-1, r);
          if (a[i-1] < m) {
             r[indiceR] = a[i-1];
             return indiceR+1;
          } else 
             return indiceR;
        }
    }

    // Wrapper
    public static int[] fMin(int[] a, int m) {
      int[] r = new int[contaMinori(a, m, 0, a.length)];
      fMin(a, m, a.length, r); 
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
System.out.println(uguali(new int[] {}     , fMin(new int[] {}       , 1)));
System.out.println(uguali(new int[] {0}    , fMin(new int[] {0}      , 1)));
System.out.println(uguali(new int[] {}     , fMin(new int[] {1}      , 1)));
System.out.println(uguali(new int[] {0}    , fMin(new int[] {0,1}    , 1)));
System.out.println(uguali(new int[] {0}    , fMin(new int[] {1,0}    , 1)));
System.out.println(uguali(new int[] {0,0,0}, fMin(new int[] {0,0,0}  , 1)));
System.out.println(uguali(new int[] {0,0}  , fMin(new int[] {0,1,0}  , 1)));
System.out.println(uguali(new int[] {}     , fMin(new int[] {1,1,1}  , 1)));
System.out.println(uguali(new int[] {0,0}  , fMin(new int[] {0,1,0,2}, 1)));
System.out.println(uguali(new int[] {0,0}  , fMin(new int[] {1,0,1,0}, 1)));
    }
}