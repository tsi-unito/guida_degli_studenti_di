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
 * @brief Initializes 1 semaphore with a given key
 *
 * @param sem_key The key used to create / connect to the sem. group
 * @param value The initial value
 * @return int The sem. unique ID.
 */
int sem_init(size_t sem_key, int value);

/**
 * @brief Connects to 1 semaphore (0) with a given key
 *
 * @param sem_key The key used to connect to the sem. group
 * @return int The sem. unique ID.
 */
int sem_connect(size_t sem_key);

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

void print_error(char *str);
void print_info(char *str);