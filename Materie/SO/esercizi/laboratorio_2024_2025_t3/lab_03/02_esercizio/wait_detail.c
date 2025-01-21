#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

/**
 * DOMANDE:
 * --------
 * (1) Che cosa restituisce la system call wait? (man -s  2  wait)
 * 	R: un pid_t che contiene il PID del primo processo figlio che e'
 * terminato, -1 altrimenti in caso di errore. (2) A cosa serve l’argomento
 * della wait? (man -s  2  wait) R: serve al chiamante per avere lo status di
 * ritorno del primo processo che e' terminato. (3) In che senso padre e figlio
 * "condividono" le variabili? R: Le "condividono" nel senso che i segmenti
 * Data, Stack e Heap vengono COPIATI verso il figlio, per cui Il figlio puo'
 * accedere ad una loro COPIA (4) Dichiarare una variabile intera myvar e, prima
 * della fork(), assegnarle un valore X. Modificare il valore di myvar nel corpo
 * del figlio. Stampare myvar nel codice del padre prima e dopo la wait(). Che
 * valori di X possiamo aspettarci? Perché? R: Nel parent il valore di my_var
 * rimane invariato perche' il figlio opera su una copia dello Stack e non nello
 * stesso stack. Stessa cosa si puo' dire del figlio. (5) Cosa succede se il
 * processo padre non attende la terminazione del figlio con una wait()? R:
 * Succede che poi il padre terminera' e il processo figlio rimarra' orfano. Da
 * li il processo padre di tutti con PID = 1 lo "adottera'".
 */

int main() {
    int my_var = 42;
    printf("[parent]: assigned my_var: %d\n", my_var);

    switch (fork()) {
        // fork in errore
        case -1:
            fprintf(stderr, "La fork() e' andata in errore!\n");
            return 1;
            break;
        // figlio
        case 0:
            // il figlio deve modificare il corpo della funzione
            my_var = 10;
            printf("[child with PID %d]: changed my_var: %d\n", getppid(),
                   my_var);
            exit(0);
            break;
        // padre
        default:
            // aspetto il figlio
            int children_stat = -1;
            pid_t wait_result = wait(&children_stat);

            if (wait_result < 0) {
                fprintf(stderr,
                        "La wait del processo figlio e' andata in errore!\n");
                exit(1);
            }

            printf("[parent] retrieved my_var: %d\n", my_var);
            break;
    }

    return 0;
}
