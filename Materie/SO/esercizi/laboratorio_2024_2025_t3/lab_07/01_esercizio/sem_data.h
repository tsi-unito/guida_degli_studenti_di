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