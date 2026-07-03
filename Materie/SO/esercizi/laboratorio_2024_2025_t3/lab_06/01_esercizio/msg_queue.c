#include <stdio.h>
#include <stdlib.h>

// System V imports
#include <errno.h>
#include <string.h>
#include <sys/msg.h>
#include <sys/types.h>

typedef struct msqid_ds *MsgQDataStr;

// structure for the single message
#define MAX_MSG_SIZE 500
typedef struct {
    long mtype;               /* message type, must be > 0 */
    char mtext[MAX_MSG_SIZE]; /* message data */
} StrMessage;
typedef StrMessage *Message;

// functions
int msg_queue_create();
MsgQDataStr msg_queue_get_info(int msg_key);
void msg_queue_print(MsgQDataStr info);
void msg_queue_close(int msg_key);
void msg_queue_send(int msg_key, char *message);

int main() {
    // creating the key
    int msg_key = msg_queue_create();

    // getting the msgq_id_ds data structure
    MsgQDataStr info_str = msg_queue_get_info(msg_key);
    msg_queue_print(info_str);
    free(info_str);

    // sending a message in the queue
    msg_queue_send(msg_key, "helloooo");

    // getting the updated info
    info_str = msg_queue_get_info(msg_key);
    msg_queue_print(info_str);
    free(info_str);

    // closing the queue
    msg_queue_close(msg_key);

    return 0;
}

int msg_queue_create() {
    // creating the queue
    // the second parameter also includes permissions
    // - 1st bit is for special flags
    // - 2nd bit is for the user permissions (rwx) in octal
    // - 3rd bit is for the user permissions (rwx) in octal
    // - 4th bit is for the user permissions (rwx) in octal
    int msg_key = msgget(IPC_PRIVATE, IPC_CREAT | 0666);

    if (msg_key == -1) {
        fprintf(stderr, "[ERROR]: Message queue creation failed! '%s'\n",
                strerror(errno));
        exit(1);
    }

    printf("[INFO]: Message queue created successfully! ID: '%d'\n", msg_key);

    return msg_key;
}

MsgQDataStr msg_queue_get_info(int msg_key) {
    printf("[INFO]: Getting information from queue with id '%d'\n", msg_key);

    // allocating memory for the message queue data structure;
    MsgQDataStr infostr = (MsgQDataStr)malloc(sizeof(struct msqid_ds));
    if (infostr == NULL) {
        fprintf(
            stderr,
            "[ERROR]: Could not allocate memory for the queue data structure! "
            "'%s'\n",
            strerror(errno));
        exit(1);
    }

    int ctl_result = msgctl(msg_key, IPC_STAT, infostr);

    if (ctl_result == -1) {
        fprintf(stderr,
                "[ERROR]: Could not retrieve the message queue data! '%s'\n",
                strerror(errno));
        exit(1);
    }

    return infostr;
}

void msg_queue_print(MsgQDataStr info) {
    if (info == NULL) {
        fprintf(stderr, "[ERROR]: The info struct was NULL!\n");
        return;
    }

    printf("[INFO]: Queue details\n");
    printf("\tQueue dim: %ld\n", info->msg_qbytes);
    printf("\tQueue messages: %ld\n", info->msg_qnum);
    printf("\tLast send (time): %ld\n", info->msg_stime);
}

void msg_queue_close(int msg_key) {
    // The flag IPC_RMID closes the flag
    int ctl_result = msgctl(msg_key, IPC_RMID, NULL);

    if (ctl_result == -1) {
        fprintf(stderr, "[ERROR]: Could not close the key! '%s'\n",
                strerror(errno));
        exit(1);
    }

    printf("[INFO]: Successfully closed message queue with id: %d\n", msg_key);
}

void msg_queue_send(int msg_key, char *message) {
    printf("[INFO]: Sending message '%s' in the queue with id '%d'\n", message,
           msg_key);

    size_t str_size = strlen(message) + 1;

    if (str_size > MAX_MSG_SIZE) {
        printf(
            "[ERROR]: Cannot send a message with length greater than the "
            "allowed size (%ld vs %d)\n",
            str_size, MAX_MSG_SIZE);
        return;
    }

    // allocating message object
    Message msg = (Message)malloc(sizeof(StrMessage));
    msg->mtype = 1;
    strcpy(msg->mtext, message);

    // Sending the message by:
    // - msg_key: The ID of the message queue
    // - msg: The message pointier
    // - str_size: the size of the message text
    // - 0: additional flags
    int result = msgsnd(msg_key, msg, str_size, 0);

    if (result == -1) {
        fprintf(
            stderr,
            "[ERROR]: Could not send the message to the queue with id '%d'! "
            "Msg: '%s'\n",
            msg_key, strerror(errno));
        free(msg);
        return;
    }

    free(msg);
    printf(
        "[INFO]: Successfully sent the message '%s' in the message queue with "
        "id '%d'!",
        message, msg_key);
}
