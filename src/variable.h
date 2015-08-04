#ifndef VARIABLE_H_  
#define VARIABLE_H_

#include "Rinternals.h"
#include "R.h"

// Variable structure defintion
struct variable {
  double* data; // variable contents
  int size;     // variable size
  int* order;   // sorted index
};

// create, initialize and return variables
struct variable* variable_factory(double* data, int size, double* sv, int sv_size);

// release variable storage
void release_variable(struct variable* v);

// create pointer to array of sorted index values
void sort_variable_index(struct variable* v);

void print_variable(struct variable* v);

#endif // VARIABLE_H_