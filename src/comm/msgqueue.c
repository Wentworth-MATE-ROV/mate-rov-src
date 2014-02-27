// Joe Jevnik
// 2014.2.27
// The message queue to run in a thread that will manage reading and writing to
// arduino.

#include "comm.h"

// Initializes a node with the given message.
void init_node(rov_node* n,unsigned char* msg,size_t len){
    n->msg = malloc(len * sizeof(unsigned char));
    memcpy(n->msg,msg,len);
    n->len         = len;
    n->is_blocking = false;
    n->tail        = NULL;
}


// Initializes an empty queue.
void init_queue(rov_msgqueue *q,rov_arduino *a,useconds_t sleep_time,size_t r){
    q->arduino    = a;
    q->head       = NULL;
    q->last       = NULL;
    q->size       = 0;
    q->is_waiting = false;
    q->response   = 0;
    q->miswrites  = 0;
    q->sleep_time = sleep_time;
    q->r_attempts = r;
    pthread_mutex_init(&q->mutex,NULL);
}

// Enqueues a message to be sent to the arduino when it is ready.
// return: The pointer to the allocated node.
rov_node *enqueue(rov_msgqueue *q,unsigned char *msg,size_t len){
    rov_node *n = malloc(sizeof(rov_node));
    init_node(n,msg,len);
    pthread_mutex_lock(&q->mutex);
    if (!q->size){
        q->head = n;
        q->last  = n;
        q->size = 1;
    }else{
        q->last->tail = n;
        q->last       = n;
        ++q->size;
    }
    pthread_mutex_unlock(&q->mutex);
    return n;
}

// Enqueues a message and blocks the caller for a response.
// return: The response from the arduino.
unsigned short enqueue_blocking(rov_msgqueue *q,unsigned char *msg,size_t len){
    rov_node *n = enqueue(q,msg,len);
    pthread_mutex_lock(&q->mutex);
    n->is_blocking = true;
    while (n->is_blocking);
    pthread_mutex_unlock(&q->mutex);
    return q->response;
}

// Dequeues the next message and sends it.
// return: the staus of the underlying write call.
int dequeue(rov_msgqueue *q){
    rov_node *n;
    unsigned short res;
    int r,c;
    pthread_mutex_lock(&q->mutex);
    assert(q->size > 0);
    n = q->head;
    r = write_str(q->arduino,n->msg,n->len);
    for (c = 0;r && c < q->r_attempts;c++){
        ++q->miswrites;
        r = write_str(q->arduino,n->msg,n->len);
    }
    if (n->is_blocking){
        while (!read(q->arduino->fd,&res,sizeof(unsigned short))){
            usleep(500);
        }
        q->response    = res;
        n->is_blocking = false;
    }
    n = n->tail;
    free(q->head->msg);
    free(q->head);
    q->head = n;
    --q->size;
    pthread_mutex_unlock(&q->mutex);
    return r;
}

// Procedure to run in a pthread to manage the message queue.
// Accepts the rov_msgqueue* as a void*.
void *process_queue(void *v_q){
    rov_msgqueue *q = v_q;
    for (;;){
        if (q->size > 0 && !q->is_waiting){
            if (dequeue(q)){
                fputs("Unable to sends a command, ignoring!",stderr);
            }
        }
        if (q->is_waiting){
            q->is_waiting = poll_shouldstart(q->arduino);
        }else{
            q->is_waiting = !poll_shouldwait(q->arduino);
        }
        usleep(q->sleep_time);
    }
    return NULL;
}
