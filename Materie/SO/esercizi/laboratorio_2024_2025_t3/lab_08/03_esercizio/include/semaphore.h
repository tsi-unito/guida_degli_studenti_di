// this file is used to include the key of the semaphore for all
// the files.
#define SEM_KEY 42
#define SEM_PERMS 0777

/**
 * @brief Creates a new semaphore
 *
 * @param sem_key The given semaphore key
 * @param val The given initial value
 * @return size_t The ID of the created semaphore.
 */
size_t sem_get(size_t sem_key, size_t val);

/**
 * @brief Signal for a semaphore.
 *
 * @param sem_id The given semaphore ID.
 */
void sem_signal(size_t sem_id);

/**
 * @brief Wait for semaphore
 *
 * @param sem_id The given sem. ID.
 */
void sem_wait(size_t sem_id);

/**
 * @brief Deletes a given semaphore
 *
 * @param sem_id The semaphore id
 */
void sem_del(size_t sem_id);