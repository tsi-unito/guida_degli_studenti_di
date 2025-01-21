#include <sys/types.h>

#define SHM_KEY 42
#define SHM_PERM 0777

typedef struct {
    pid_t pid;
} StrMessage;

typedef StrMessage *Message;

/**
 * @brief Creates / connects to a shared memory by a given key.
 *
 * @param shm_key The given key
 * @param shm_size The size of the shared memory.
 * @return int The shared memory's unique ID.
 */
size_t shm_get(size_t shm_key);

/**
 * @brief Attaches to a shared memory.
 *
 * @param shm_id The shared memory's ID
 * @return Message The given message.
 */
Message shm_attach(size_t shm_id);

/**
 * @brief Detaches a given message.
 *
 * @param msg The given message.
 */
void shm_detach(Message msg);

/**
 * @brief Deletes the shared memory.
 *
 * @param shm_id The given Shared memory ID.
 */
void shm_del(size_t shm_id);