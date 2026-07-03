#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// message queue const and imports
#define MSGQ_FLAGS 0666
#define MSG_MAX_LEN 500
#include <sys/msg.h>
#include <sys/types.h>

typedef struct {
    long mtype;
    char mtext[MSG_MAX_LEN];
} StrMessage;

typedef StrMessage *Message;

void print_menu();

/**
 * Connects to an existing queue.
 * @returns The message queue unique ID.
 * */
int queue_connect();

/**
 * Gets a single message from the queue.
 * @param msgq_id - The ID of the message queue.
 * */
void queue_get_single_message(int msgq_id);

int main() {
    short choice;
    int msgq_id = -1;
    do {
        print_menu();
        scanf("%hd", &choice);

        switch (choice) {
            case 0:  // exit
                printf("Exit selected! Closing..\n");
                break;
            case 1:  // connect to queue
                if (msgq_id >= 0) {
                    fprintf(stderr,
                            "[ERROR]: You already have connected to a queue "
                            "with ID '%d'\n",
                            msgq_id);
                } else {
                    msgq_id = queue_connect();
                }
                break;
            case 2:  // get a single message
                if (msgq_id == -1) {
                    fprintf(
                        stderr,
                        "[ERROR]: You don't have connected to any queue!\n");
                } else {
                    queue_get_single_message(msgq_id);
                }
                break;
        }
    } while (choice != 0);

    return 0;
}

void print_menu() {
    printf("---------------------------\n");
    printf("0. Exit.\n");
    printf("1. Connect to queue.\n");
    printf("2. Get a single message.\n");
    printf("---------------------------\n");
    printf("Your choice: ");
}

int queue_connect() {
    key_t msgq_key;
    printf("Insert the KEY of the message queue: ");
    scanf("%d", &msgq_key);

    // connecting
    int msgq_id = msgget(msgq_key, MSGQ_FLAGS);
    if (msgq_id == -1) {
        fprintf(stderr,
                "[ERROR]: Could not connect to the message queue! '%s'\n",
                strerror(errno));
        exit(1);
    }

    printf(
        "[INFO]: successfully connected to queue with key '%d' and ID '%d'\n",
        msgq_key, msgq_id);
    return msgq_id;
}

void queue_get_single_message(int msgq_id) {
    printf("[INFO]: Getting a message from the message queue with id '%d'\n",
           msgq_id);

    int msg_type;
    printf("What type of message do you want? [0 = All]: ");
    scanf("%d", &msg_type);

    // allocating memory
    Message msg = (Message)malloc(sizeof(StrMessage));
    if (msg == NULL) {
        fprintf(stderr,
                "[ERROR]: Could not allocate memory for the message! '%s'\n",
                strerror(errno));
    }

    ssize_t rcv_res = msgrcv(msgq_id, msg, MSG_MAX_LEN, msg_type, 0);

    if (rcv_res == -1) {
        fprintf(stderr,
                "[ERROR]: Could not get the message from the queue! '%s'\n",
                strerror(errno));
    }

    printf("[INFO]: Got the following message from the queue:\n");
    printf("\tText: %s\n", msg->mtext);
    printf("\tType: %ld\n", msg->mtype);
}
