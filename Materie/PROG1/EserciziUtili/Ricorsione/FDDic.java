import java.util.Arrays;

/** Scrivere un metodo fD con le seguenti caratteristiche:
  - fD ha un parametro formale a di tipo array di interi;
  - fD restituisce un array che contiene tutti e soli gli interi dispari inizialmente in a, rispettando l'ordine originale degli elementi;
  - fD è wrapper di un paio di metodi ricorsivi tali che:
      + il primo è dicotomico e conta quanti elementi in a dovranno essere restituiti nell'array risultato;
      + il secondo è contro-variante e risolve effettivamente il problema.
  Per ipotesi, l'array a passato al metodo fD come parametro attuale non puo' essere null. 
  NOTA. Il metodo con firma boolean Arrays.equals(int[],int[]) restituisce true se i due parametri
  attuali sono array identici elemento per elemento, e false altrimenti. */
  
public class FDDic {

    // Dicotomico
    public static int contaDispari(int[] a, int l, int r) {
      if (r-l == 0) 
        return 0;
      else
        if (r-l == 1)
          if (a[l]%2 == 1) 
                return 1;
             else
                return 0;
        else {
          int m = (l+r)/2;
          return contaDispari(a, l, m) + contaDispari(a, m, r);
        }
    }

    // Contro-variante
    public static int fD(int[] a, int i, int[] r) {
        if (i == a.length)
          return 0;
        else {
          int indiceR = fD(a, i+1, r);
          if (a[i]%2 == 1) {
             r[indiceR] = a[i];
             return indiceR+1;
          } else 
             return indiceR;
        }
    }

    // Wrapper
    public static int[] fD(int[] a) {
      int[] r = new int[contaDispari(a, 0, a.length)];
      fD(a, 0, r); 
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