#ifndef VARIABLE_H
#define VARIABLE_H

#include "variable.h"
#include "stdlib.h"

// global to store the currently processed array of values
double* base_array = NULL;

// create and initalized a variable struct
struct variable* variable_factory(double* data, int size, double* sv, int sv_size) {
  
  // don't use NAs
  size_t num_real = 0, num_special = 0;
  for (size_t i = 0; i < size; i++){
    num_real += !ISNA(data[i]);
    for (size_t j = 0; j < sv_size; j++){
      num_special += (data[i] == sv[j]);
    }
  }
  Rprintf("Number of real values: %d\n", num_real);
  Rprintf("Number of special values: %d\n", num_special);
  
  
  struct variable* v = malloc(sizeof(*v));
  v->data = data;
  v->size = num_real;
  
  v->order = malloc(sizeof(int) * num_real); // create storage for index
  
  // initialize v->order with sequence
  for(size_t i = 0, j = 0; i < size; i++) {
    if (!ISNA(data[i])) {
      v->order[j] = i;
      j++;
    }
  }
  
  sort_variable_index(v); // create sorted index
  
  return v;
}

// release storage for variable
void release_variable(struct variable* v) {
  free(v->order);
  free(v);
}

// compare function for sorting index
int compare(const void* a, const void* b) {
  int aa = *(int*)a;
  int bb = *(int*)b;
  
  return
     (base_array[aa] < base_array[bb]) ? -1
    :(base_array[aa] > base_array[bb]) ?  1
    : 0;
}

// sort the 
void sort_variable_index(struct variable* v) {
  // put data array into global temporarily and sort it
  base_array = v->data;
  qsort(v->order, v->size, sizeof(v->order[0]), compare);
}

void print_variable(struct variable* v){
  for(size_t i = 0; i < v->size; i++) {
    Rprintf("Sorted Vec: %f\n", v->data[v->order[i]]);
  }
}

#endif