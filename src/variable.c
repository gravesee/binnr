#include "variable.h"
#include "stdlib.h"

// global to store the currently processed array of values
double* base_array = NULL;

// create and initalized a variable struct
variable* variable_factory(double* data, int size) {
  
  variable* v = malloc(sizeof(variable));
  v->data = data;
  v->size = size;
  
  v->order = malloc(sizeof(int) * size); // create storage for index
  // initialize v->order with sequence
  for(size_t i = 0; i < v->size; i++) {
    v->order[i] = i;
  }
  
  sort_variable_index(v); // create sorted index
  
  return v;
}

// release storage for variable
void release_variable(variable* v) {
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
void sort_variable_index(variable* v) {
  
  // put data array into global temporarily and sort it
  base_array = v->data;
  qsort(v->order, v->size, sizeof(v->order[0]), compare);
}

void print_variable(variable* v){
  for(size_t i = 0; i < v->size; i++) {
    Rprintf("Sorted Vec: %f\n", v->data[v->order[i]]);
  }
}
