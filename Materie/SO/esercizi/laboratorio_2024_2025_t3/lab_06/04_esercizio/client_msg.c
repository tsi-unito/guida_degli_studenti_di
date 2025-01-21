#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#define DEF_CLIENTS 1

// imports for message queue
#include <sys/msg.h>
#include <sys/types.h>
#define MSG_TXT_LEN 500
#define MSGQ_FLAGS 0777
#define MSG_KEY 42
#define MSG_SIZE sizeof(StrMessage) - sizeof(long)
#define MSG_RES getpid()
#define MSG_REQ 2
#define MSG_CLOSE 1

typedef struct {
    long mtype;
    char mtext[MSG_TXT_LEN];
    long response_channel;
} StrMessage;

typedef StrMessage *Message;

/**
 * @brief Connects to a given queue and returns its unique ID.
 *
 * @return int The queue ID.
 */
int queue_connect(int msgq_key);

/**
 * @brief Prints info about a message
 *
 * @param msg The given message.
 */
void message_print(Message msg);

/**
 * @brief Sends a message to a queue
 *
 * @param msgq_id The given ID of the queue
 */
void queue_send_req(int msgq_id);

/**
 * @brief Gets a message from a queue identified by it's ID.
 *
 * @param msgq_id The given ID
 * @return Message An instance of the message.
 */
Message queue_receive_res(int msgq_id);

/**
 * @brief Sends the termination message to the queue.
 *
 * @param msgq_id The ID of the queue to send the termination message.
 */
void queue_send_term(int msgq_id);

/**
 * @brief Retrieves the # of callers from the input
 *
 * @param argc The argc
 * @param argv The argv
 * @return int The # of processes to create as callers
 */
int get_callers(int argc, char **argv);

int main(int argc, char **argv) {
    int msgq_id = queue_connect(MSG_KEY);
    int num_proc = get_callers(argc, argv);

    // sending requests for every process
    pid_t parent_pid = getpid();
    for (size_t i = 0; i < num_proc; i++) {
        if (getpid() == parent_pid) {
            switch (fork()) {
                case -1:  // error
                    fprintf(stderr, "[ERROR - PID: %d]: Could not fork! '%s'\n",
                            parent_pid, strerror(errno));
                    break;
                case 0:  // children
                    queue_send_req(msgq_id);

                    // waiting for response
                    Message msg = queue_receive_res(msgq_id);
                    message_print(msg);
                    free(msg);
                    exit(EXIT_SUCCESS);
                    break;
                default:  // parent
                    break;
            }
        }
    }

    while (wait(NULL) != -1);

    // sending term. message
    queue_send_term(msgq_id);

    return 0;
}

int queue_connect(int msgq_key) {
    pid_t pid = getpid();
    ssize_t key_result = msgget(msgq_key, MSGQ_FLAGS);

    if (key_result == -1) {
        fprintf(stderr, "[ERROR]: Could not allocate the message queue: '%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf(
        "[INFO - PID: %d]: Successfully connected to a queue with ID: '%ld'\n",
        pid, key_result);

    return key_result;
}

void queue_send_req(int msgq_id) {
    // allocate mem. for message
    pid_t pid = getpid();
    Message msg = (Message)malloc(sizeof(StrMessage));
    if (msg == NULL) {
        printf(
            "[ERROR - PID: %d]: Could not allocate memory for the request "
            "message! '%s'\n",
            pid, strerror(errno));
        return;
    }

    msg->mtype = MSG_REQ;
    msg->response_channel = MSG_RES;
    strcpy(msg->mtext, "Request from client!");

    printf(
        "[INFO - PID: %d]: Will now send the request message to the queue "
        "with id '%d': \n",
        pid, msgq_id);
    message_print(msg);
    int snd_res = msgsnd(msgq_id, msg, MSG_SIZE, 0);

    if (snd_res == -1) {
        fprintf(stderr,
                "[ERROR - PID: %d]: Could not send the request message! '%s'\n",
                pid, strerror(errno));
        free(msg);
        return;
    }

    printf("[INFO - PID: %d]: Request message correctly sent!\n", pid);
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
        "[INFO - PID: %d]: Message data:\n \tText: %s\n\tType: "
        "%ld\n\tResponse Channel: %ld\n",
        pid, msg->mtext, msg->mtype, msg->response_channel);
}

Message queue_receive_res(int msgq_id) {
    pid_t pid = getpid();
    // allocate memory
    Message msg = (Message)malloc(sizeof(StrMessage));
    if (msg == NULL) {
        return NULL;
    }

    ssize_t res_rcv = msgrcv(msgq_id, msg, MSG_SIZE, MSG_RES, 0);
    if (res_rcv == -1) {
        fprintf(
            stderr,
            "[ERROR - PID: %d]: Could not get the response message from the "
            "queue! '%s'\n",
            pid, strerror(errno));
        free(msg);
        return NULL;
    }

    printf(
        "[INFO - PID: %d]: Successfully retrieved the response message from "
        "the queue!\n",
        pid);
    return msg;
}

void queue_send_term(int msgq_id) {
    pid_t pid = getpid();
    // allocate mem. for message
    Message msg = (Message)malloc(sizeof(StrMessage));
    if (msg == NULL) {
        printf(
            "[ERROR - PID: %d]: Could not allocate memory for the termination "
            "message! '%s'\n",
            pid, strerror(errno));
        return;
    }

    msg->mtype = MSG_CLOSE;
    msg->response_channel = MSG_RES;
    strcpy(msg->mtext, "Termination message!");

    printf(
        "[INFO - PID: %d]: Will now send the termination message to the queue "
        "with id '%d': \n",
        pid, msgq_id);
    message_print(msg);
    int snd_res = msgsnd(msgq_id, msg, MSG_SIZE, 0);

    if (snd_res == -1) {
        fprintf(
            stderr,
            "[ERROR - PID: %d]: Could not send the termination message! '%s'\n",
            pid, strerror(errno));
        free(msg);
        return;
    }

    printf("[INFO - PID: %d]: Termination message correctly sent!\n", pid);
}

int get_callers(int argc, char **argv) {
    pid_t pid = getpid();
    if (argc != 2) {  // gracefully return 1
        printf(
            "[WARN - PID: %d]: Wrong count of arguments detected! Proceeding "
            "with the defaults! # of processes: %d\n",
            pid, DEF_CLIENTS);
        return DEF_CLIENTS;
    }

    int parsed_val = atoi(argv[1]);

    if (parsed_val < 1) {
        printf(
            "[WARN - PID: %d]: Wrong value of # of processes detected! "
            "Proceeding with the defaults! # of processes: %d\n",
            pid, DEF_CLIENTS);
        return DEF_CLIENTS;
    }

    return parsed_val;
}