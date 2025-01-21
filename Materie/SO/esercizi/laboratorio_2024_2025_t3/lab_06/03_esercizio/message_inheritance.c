#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// imports for forking
#include <sys/wait.h>
#include <unistd.h>

// imports and structs for the message queue
#include <sys/msg.h>
#include <sys/types.h>
#define MSG_FLAGS 0666
#define MSG_MAX_LEN 100

/**
 * @brief Defines the struct of a single message.
 */
typedef struct {
    long mtype;
    char mtext[MSG_MAX_LEN];
} StrMessage;

/**
 * @brief Defines a pointier to a single message.
 *
 */
typedef StrMessage *Message;

/**
 * @brief Creates a new system V message queue.
 *
 * @return int The unique queue identifier.
 */
int queue_create();

/**
 * @brief Gets a message from a given queue.
 *
 * @param pid The given PID of the process.
 * @param msg_id The given queue ID.
 */
void queue_receive(pid_t pid, int msg_id);

/**
 * @brief Closes a given queue.
 *
 * @param pid The given PID of the process.
 * @param msg_id The given queue ID.
 */
void queue_close(pid_t pid, int msg_id);

/**
 * @brief Sends a message in the queue.
 *
 * @param msg_id The given queue ID.
 */
void queue_send(int msg_id);

int main() {
    int msg_id = queue_create();

    // forking
    pid_t pid = fork();
    switch (pid) {
        case -1:  // error
            fprintf(stderr, "[ERROR]: Could not fork a new process! '%s'\n",
                    strerror(errno));
            break;
        case 0:  // children
            printf("[INFO]: Children process! PID '%d'\n", getpid());
            queue_send(msg_id);
            break;
        default:  // parent
            pid_t parent_pid = getpid();
            printf("[INFO]: Parent process! PID '%d'\n", getpid());
            queue_receive(parent_pid, msg_id);
            queue_close(parent_pid, msg_id);
            wait(NULL);
            break;
    }

    return 0;
}

// Docs available in prototype
int queue_create() {
    int msg_id = msgget(IPC_PRIVATE, IPC_CREAT | MSG_FLAGS);

    if (msg_id == -1) {
        fprintf(stderr, "[ERROR]: Could not create the message queue! '%s'\n",
                strerror(errno));
        exit(1);
    }

    printf("[INFO]: Correctly created a message queue with ID: '%d'\n", msg_id);

    return msg_id;
}

// Docs available in prototype
void queue_receive(pid_t pid, int msg_id) {
    printf(
        "[INFO - PID: %d] - Trying to receive messages from queue with ID "
        "'%d'\n",
        pid, msg_id);
    // allocating memory for the message
    Message msg = (Message)malloc(sizeof(StrMessage));
    if (msg == NULL) {
        fprintf(
            stderr,
            "[ERROR - PID: %d]: Could not allocate memory for the message to "
            "receive! '%s'\n",
            pid, strerror(errno));
        return;
    }

    const long type = 0;  // get any message
    ssize_t res_rcv = msgrcv(msg_id, msg, MSG_MAX_LEN, type, 0);

    if (res_rcv == -1) {
        fprintf(
            stderr,
            "[ERROR - PID: %d]: Could not receive the message from the queue! "
            "'%s'\n",
            pid, strerror(errno));
        free(msg);
        queue_close(pid, msg_id);
        return;
    }

    printf("[INFO - PID: %d]: Successfully received a message! Message data:\n",
           pid);
    printf("\tText: %s\n", msg->mtext);
    printf("\tType: %ld\n", msg->mtype);
    free(msg);
}

// Docs available in prototype
void queue_close(pid_t pid, int msg_id) {
    printf("[INFO - PID: %d]: Now closing the queue with ID '%d'\n", pid,
           msg_id);
    int res_cls = msgctl(msg_id, IPC_RMID, NULL);

    if (res_cls == -1) {
        fprintf(
            stderr,
            "[ERROR - PID: %d]: Could not close the queue with ID '%d': '%s'",
            pid, msg_id, strerror(errno));
        return;
    }
    printf("[INFO - PID: %d]: successfully closed the queue with ID '%d'\n",
           pid, msg_id);
}

// Docs available in prototype
void queue_send(int msg_id) {
    pid_t pid = getpid();
    printf(
        "[INFO - PID: %d]: Now sending a message in the queue with ID '%d'\n",
        pid, msg_id);

    // allocating message
    Message msg = (Message)malloc(sizeof(StrMessage));
    if (msg == NULL) {
        fprintf(
            stderr,
            "[ERROR - PID: '%d']: Could not allocate memory for the message to "
            "send! '%s'\n",
            pid, strerror(errno));
        return;
    }

    // populating message
    char text_message[MSG_MAX_LEN];
    sprintf(text_message, "Hello from PID '%d'\n", pid);
    long type_message = 1;
    msg->mtype = type_message;
    strcpy(msg->mtext, text_message);

    printf("[INFO - PID: %d]: Sending message with text '%s' and type '%ld'\n",
           pid, msg->mtext, msg->mtype);

    // sending
    int res_snd = msgsnd(msg_id, msg, MSG_MAX_LEN, 0);
    if (res_snd == -1) {
        fprintf(
            stderr,
            "[ERROR - PID: %d]: Could not send the message in the queue! '%s': "
            "'%s'\n",
            pid, strerrorname_np(errno), strerror(errno));
        free(msg);
        return;
    }

    free(msg);

    printf("[INFO - PID: %d]: Successfully sent a message in the queue!\n",
           pid);
}