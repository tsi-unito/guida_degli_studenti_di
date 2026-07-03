#include <sys/sem.h>

// this file is used to include the key of the semaphore for all
// the files.
#define SEM_KEY 42
#define SEM_PERMS 0777

/**
 * @brief Union structure used in the semctl operation
 *
 */
union semun {
    int val;
    struct semid_ds *buf;
    unsigned short *array;
};

/**
 * @brief Typedef to define more easily the pointier to a sembuf
 *
 */
typedef struct sembuf *SemOp;

/**
 * @brief Executes a wait() / P / Require on a given semaphore
 *
 * @param sem_id The given semaphore ID
 */
void sem_wait(size_t sem_id);

/**
 * @brief Executes a signal() / V / Release on a given semaphore
 *
 * @param sem_id The given semaphore ID
 */
void sem_signal(size_t sem_id);

/**
 * @brief Prints an error on std with diagnostic data
 *
 * @param str The given string to print.
 */
void print_error(char *str);

/**
 * @brief Prints information
 *
 * @param str The string to print
 */
void print_info(char *str);