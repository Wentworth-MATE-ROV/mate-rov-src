/* msgqueue.c --- The functions that manage the queue of messages to send to the
                  arduino when needed.
   Copyright (c) Joe Jevnik

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along with
   this program; if not, write to the Free Software Foundation, Inc., 51
   Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

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
void init_queue(rov_msgqueue *q,rov_arduino *a,useconds_t sleep_time,
                size_t r_attempts){
    q->arduino    = a;
    q->head       = NULL;        // The first message.
    q->last       = NULL;        // The last message.
    q->size       = 0;           // The size of the queue (number of msgs).
    q->is_waiting = false;       // Is this waiting for the arduino buffer?
    q->response   = 0;           // Store the response back from the arduino.
    q->miswrites  = 0;           // Number of miswrites (failures).
    q->writes     = 0;           // Total writes.
    q->sleep_time = sleep_time;  // The amount of time to wait between msgs.
    q->r_attempts = r_attempts;  // Retry attempts.
    pthread_mutex_init(&q->mutex,NULL);
}

// Frees the message queue's mesages and mutex.
void destroy_queue(rov_msgqueue *q){
    rov_node *n;
    for (n = q->head;n;n = q->head->tail){
        free(n);  // Free the nodes.
    }
    pthread_mutex_destroy(&q->mutex);
}

// Enqueues a message to be sent to the arduino when it is ready.
// return: The pointer to the allocated node.
rov_node *enqueue(rov_msgqueue *q,unsigned char *msg,size_t len){
    rov_node *n = malloc(sizeof(rov_node));
    init_node(n,msg,len);
    pthread_mutex_lock(&q->mutex);
    if (!q->size){
        q->head = n;
        q->last = n;
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
    n->is_blocking = true;
    while (n->is_blocking);
    return q->response;
}

// Dequeues the next message and sends it.
// return: the staus of the underlying write call.
int dequeue(rov_msgqueue *q){
    rov_node      *n;
    unsigned short res;
    unsigned int   r,c;
    pthread_mutex_lock(&q->mutex);
    assert(q->size > 0);
    n = q->head;
    r = write_str(q->arduino,n->msg,n->len);
    for (c = 0;r && c < q->r_attempts;c++){
        r = write_str(q->arduino,n->msg,n->len);
    }
    if (n->is_blocking){
        while (!read(q->arduino->fd,&res,sizeof(unsigned short))){
            usleep(200); // Magic timing number to not kill my processor.
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
void *process_queue(void *pqp){
    rov_pq_param *ps  = pqp;
    rov_msgqueue *q   = ps->q;
    rov_screen   *scr = ps->scr;
    char          w;
    for (;;){
        if (q->size > 0 && !q->is_waiting){
            if (dequeue(q)){
                screen_printfattr(scr,RED_PAIR,"Miswrite occured (total: %d)",
                                  ++q->miswrites);
            }else{
                ++q->writes;
            }
        }
        w = poll_wait(q->arduino);
        if (q->is_waiting && w == OP_SHOULDSTART){
            q->is_waiting = false;
            screen_printattr(scr,GREEN_PAIR,"Recieved: OP_SHOULDSTART");
        }else if (!q->is_waiting && w == OP_SHOULDWAIT){
            q->is_waiting = true;
            screen_printattr(scr,RED_PAIR,"Recieved: OP_SHOULDWAIT");
        }
        usleep(q->sleep_time);  // Sleep the thread.
    }
    return NULL;
}
