#include "Rinternals.h"
#include "R.h"
#include "Rmath.h"
#include "stdio.h"
#include "stdlib.h"
#include "variable.h"

SEXP print_vec(SEXP x, SEXP y) {
  
  double* dx = REAL(x);
  double* dy = REAL(y);
  
  variable* v1 = initialize_variable(dx, 1000);
  variable* v2 = initialize_variable(dy, 1000);
  
  for (int i = 0; i < v1->size; i++) {
    Rprintf("Value: %f\t%f\n", v1->data[i], v2->data[i]);
  }
  
  free(v1);
  free(v2);
  
  return R_NilValue;
  
}