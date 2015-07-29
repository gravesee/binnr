//
// Created by Eric Graves on 7/24/15.
//

#ifndef QUEUE_SOURCE_FILE_H
#define QUEUE_SOURCE_FILE_H

//#include "stdio.h"
#include "stdlib.h"


// functions for creating a queue of start/stop work
struct work {
    int start;
    int stop;
};

struct node {
    struct work work;
    struct node *next;
};

struct queue {
    struct node* first;
    struct node* last;
    int size;
};

//init_queue
struct queue* queue_factory();

// enqueue -- add a work struct to the queue
void enqueue(struct queue* q, struct work w);

// dequeue -- take a work struct from the queue
struct work dequeue(struct queue* q);

// size of queue
int queue_size(struct queue* q);

// print items in queue
void queue_print(struct queue* q);

// destroy queue and contents
void release_queue(struct queue* q);

// return 1 if queue is empty
int is_empty(struct queue* q);

#endif //QUEUE_SOURCE_FILE_H
