#include <stdlib.h>

#define SHM_KEY 41
#define ROW_LEN 500

typedef struct {
    char row[ROW_LEN];
} StrMessageItem;

typedef StrMessageItem *MessageItem;

/**
 * @brief Creates the shared memory segment
 *
 * @param shm_key The shared memory key
 * @param size The size of the shared memory
 * @return int The ID of the shared mem.
 */
int shm_get(size_t shm_key, size_t size);

/**
 * @brief Attaches the memory.
 *
 * @param shm_id
 * @return MessageItem The messageItem instance
 */
MessageItem shm_attach(size_t shm_id);

/**
 * @brief Detaches the memory
 *
 * @param shm_id The ID of the shared memory.
 * @param msg The message to detach.
 */
void shm_detach(size_t shm_id, MessageItem msg);

/**
 * @brief Deletes a shared memory object
 *
 */
void shm_delete(size_t shm_id);