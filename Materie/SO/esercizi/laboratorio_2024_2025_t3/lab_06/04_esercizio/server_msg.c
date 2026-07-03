#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

// imports for message queue
#include <sys/msg.h>
#include <sys/types.h>
#define MSG_TXT_LEN 500
#define MSGQ_FLAGS 0777
#define MSG_KEY 42
#define MSG_SIZE sizeof(StrMessage) - sizeof(long)
#define MSG_REQ 2
#define MSG_CLOSE 1

typedef struct {
    long mtype;
    char mtext[MSG_TXT_LEN];
    long response_channel;
} StrMessage;

typedef StrMessage *Message;

/**
 * @brief Creates a new queue and returns its unique ID.
 *
 * @return int
 */
int queue_create();

/**
 * @brief Gets a message from a queue identified by it's ID.
 *
 * @param msgq_id The given ID
 * @return Message An instance of the message.
 */
Message queue_receive(int msgq_id);

/**
 * @brief Prints info about a message
 *
 * @param msg The given message.
 */
void message_print(Message msg);

/**
 * @brief Sends a response to a given request
 *
 * @param msgq_id The ID of the message queue.
 * @param response_channel The response channel on where to send the response.
 */
void queue_send_response(int msgq_id, long response_channel);

/**
 * @brief Closes a given queue.
 *
 * @param msgq_id The ID of the message queue.
 */
void queue_close(int msgq_id);

int main() {
    int msgq_id = queue_create();

    Message msg = queue_receive(msgq_id);
    while (msg != NULL && msg->mtype != MSG_CLOSE) {
        message_print(msg);
        long response_channel = msg->response_channel;
        switch (fork()) {
            case -1:  // error
                fprintf(stderr, "[ERROR]: Could not fork! '%s'\n",
                        strerror(errno));
                break;
            case 0:  // children
                queue_send_response(msgq_id, response_channel);
                exit(EXIT_SUCCESS);
                break;
            default:  // parent
                free(msg);
                msg = queue_receive(msgq_id);
                break;
        }
    }

    while (wait(NULL) != -1);
    if (msg == NULL) {
        fprintf(stderr, "[ERROR]: Received NULL message!\n");
        exit(EXIT_FAILURE);
    }

    // closing the queue
    queue_close(msgq_id);
    return 0;
}

int queue_create() {
    pid_t pid = getpid();
    ssize_t key_result = msgget(MSG_KEY, IPC_CREAT | MSGQ_FLAGS);

    if (key_result == -1) {
        fprintf(
            stderr,
            "[ERROR - PID: %d]: Could not allocate the message queue: '%s'\n",
            pid, strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf("[INFO - PID: %d]: Successfully created a queue with ID: '%ld'\n",
           pid, key_result);

    return key_result;
}

Message queue_receive(int msgq_id) {
    pid_t pid = getpid();
    // allocate memory
    Message msg = (Message)malloc(sizeof(StrMessage));
    if (msg == NULL) {
        return NULL;
    }

    ssize_t res_rcv = msgrcv(msgq_id, msg, MSG_SIZE, -MSG_REQ, 0);
    if (res_rcv == -1) {
        fprintf(stderr,
                "[ERROR - PID: %d]: Could not get the message from the queue! "
                "'%s'\n",
                pid, strerror(errno));
        free(msg);
        return NULL;
    }

    printf(
        "[INFO - PID: %d]: Successfully retrieved a message from the queue!\n",
        pid);
    return msg;
}

void message_print(Message msg) {
    pid_t pid = getpid();
    if (msg == NULL) {
        fprintf(
            stderr,
            "[ERROR - PID: %d]: The message set to get its data was NULL!\n",
            pid);
        return;
    }

    printf(
        "[INFO - PID: %d]: Message data:\n\tText: %s\n\tType: %ld\n\tResponse "
        "Channel: %ld\n",
        pid, msg->mtext, msg->mtype, msg->response_channel);
}

void queue_send_response(int msgq_id, long response_channel) {
    pid_t pid = getpid();
    // allocate mem. for message
    Message msg = (Message)malloc(sizeof(StrMessage));
    if (msg == NULL) {
        printf(
            "[ERROR - PID %d]: Could not allocate memory for the response "
            "message! '%s'\n",
            pid, strerror(errno));
        return;
    }

    msg->mtype = response_channel;
    msg->response_channel = -1;
    strcpy(msg->mtext, "Response from server");

    printf(
        "[INFO - PID: %d]: Will now send the response message to the queue "
        "with id '%d' as response in the channel: %ld\n",
        pid, msgq_id, response_channel);
    message_print(msg);
    int snd_res = msgsnd(msgq_id, msg, MSG_SIZE, 0);

    if (snd_res == -1) {
        fprintf(stderr, "[ERROR - PID %d]: Could not send the response! '%s'\n",
                pid, strerror(errno));
        free(msg);
        return;
    }

    printf("[INFO - PID: %d]: Response correctly sent!\n", pid);
}

void queue_close(int msgq_id) {
    pid_t pid = getpid();
    printf("[INFO - PID: %d]: Now closing the queue with ID %d\n", pid,
           msgq_id);
    int res_cls = msgctl(msgq_id, IPC_RMID, NULL);

    if (msgq_id == -1) {
        fprintf(stderr, "[ERROR - PID: %d]: Could not close the queue! '%s'\n",
                pid, strerror(errno));
        return;
    }

    printf("[INFO - PID: %d]: Successfully closed the queue with id '%d'\n",
           pid, msgq_id);
}