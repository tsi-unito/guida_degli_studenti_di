#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MSG_MAX_LEN 500
#define MSGQ_FLAGS 0666

// Parte riguardante i dati dei messaggi
#include <sys/msg.h>

typedef struct {
    long mtype;
    char mtext[MSG_MAX_LEN];
} StrMessage;

typedef StrMessage *Message;

// Fine parte riguardante i dati dei messaggi

/**
 * Prints the menu for the user
 * */
void print_menu();

/**
 * Creates a new queue and returns the key id.
 * */
int queue_create();

/**
 * Sends a message in the message queue.
 * @param key_msgq - The message queue ID.
 * */
void queue_send(ssize_t key_msgq);

/**
 * Closes a message queue.
 * @param key_msgq - The message queue ID.
 * */
void queue_close(ssize_t key_msgq);

int main() {
    short choice;
    ssize_t key_msgq = -1;
    do {
        print_menu();
        scanf("%hd", &choice);
        switch (choice) {
            case 0:  // exit
                break;
            case 1:  // create queue
                if (key_msgq != -1) {
                    fprintf(
                        stderr,
                        "[ERROR]: You already have created a queue: '%ld'\n",
                        key_msgq);
                } else {
                    key_msgq = queue_create();
                }
                break;
            case 2:  // send message
                if (key_msgq == -1) {
                    fprintf(stderr,
                            "[ERROR]: You haven't created or set an existing "
                            "queue!\n");
                } else {
                    queue_send(key_msgq);
                }
                break;
            case 3:  // close queue
                if (key_msgq == -1) {
                    fprintf(stderr,
                            "[ERROR]: You haven't created or connected to an "
                            "existing queue!\n");
                } else {
                    queue_close(key_msgq);
                    key_msgq = -1;
                }
                break;
            default:
                fprintf(stderr, "[ERROR]: Invalid choice value: '%hd'\n",
                        choice);
        }
    } while (choice != 0);

    return 0;
}

// see prototype for docs
void print_menu() {
    printf("---------------------------\n");
    printf("\t\tMENU:\n");
    printf("0. Exit\n");
    printf("1. Create or access a message queue.\n");
    printf("2. Send a message\n");
    printf("3. Close a queue\n");
    printf("---------------------------\n");
}

// see prototype for docs
int queue_create() {
    char key_choice[3];
    printf("Would you like to choose the key of the message queue? [Y/N]: ");
    scanf("%2s", key_choice);

    ssize_t key_result;
    if (toupper(key_choice[0]) == 'Y') {
        key_t chosen_key;
        printf("Insert the message queue key: ");
        scanf("%d", &chosen_key);
        key_result = msgget(chosen_key, IPC_CREAT | MSGQ_FLAGS);
    } else {
        key_result = msgget(IPC_PRIVATE, MSGQ_FLAGS);
    }

    if (key_result == -1) {
        fprintf(stderr, "[ERROR]: Could not allocate the message queue: '%s'\n",
                strerror(errno));
        exit(1);
    }

    printf("[INFO]: Successfully created a queue with ID: '%ld'\n", key_result);

    return key_result;
}

// see prototype for docs
void queue_send(ssize_t key_msgq) {
    // loading the message text
    printf("Insert the message to send: ");

    char msg_txt[MSG_MAX_LEN];
    scanf("%499s", msg_txt);

    // loading the message type
    long type;
    printf("Insert the message type: ");
    scanf("%ld", &type);

    printf(
        "[INFO]: Will now send message to the queue with id '%ld': \nMessage: "
        "'%s'\nType: %ld\n",
        key_msgq, msg_txt, type);

    // allocate mem. for message
    Message msg = (Message)malloc(sizeof(StrMessage));
    if (msg == NULL) {
        printf("[ERROR]: Could not allocate memory for the message! '%s'\n",
               strerror(errno));
        return;
    }

    msg->mtype = type;
    strcpy(msg->mtext, msg_txt);

    int snd_res = msgsnd(key_msgq, msg, MSG_MAX_LEN, 0);

    if (snd_res == -1) {
        fprintf(stderr, "[ERROR]: Could not send the message! '%s'\n",
                strerror(errno));
        free(msg);
        return;
    }

    printf("[INFO]: Message correctly sent!\n");
}

// docs available in the prototype
void queue_close(ssize_t key_msgq) {
    printf("[INFO]: Closing the message queue '%ld'\n", key_msgq);

    int cls_res = msgctl(key_msgq, IPC_RMID, NULL);

    if (cls_res == -1) {
        fprintf(stderr, "[ERROR]: Could not close the queue! '%s'\n",
                strerror(errno));
    }
}
