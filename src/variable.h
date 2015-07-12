#ifndef VARIABLE_H_  
#define VARIABLE_H_

#include "Rinternals.h"
#include "R.h"

// Variable structure defintion
typedef struct {
  double* data; // variable contents
  int size;     // variable size
  int* order;   // sorted index
} variable;

// create, initialize and return variables
variable* variable_factory(double* data, int size);

// release variable storage
void release_variable(variable* v);

// create pointer to array of sorted index values
void sort_variable_index(variable* v);

void print_variable(variable* v);

#endif // VARIABLE_H_