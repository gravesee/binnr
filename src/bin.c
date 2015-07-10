#include "Rinternals.h"
#include "R.h"
#include "stdio.h"
#include "variable.h"
#include "interaction.h"

//#define DEBUG

// TODO: create a new object that combines a variable and performance

SEXP bin(SEXP x, SEXP y) {
  
  double* dx = REAL(x);
  double* dy = REAL(y);
  int sz = LENGTH(x);
  
  //Rprintf("Creating variable\n");
  variable* v1 = variable_factory(dx, sz);
  
  //Rprintf("Printing Variable\n");
  //print_variable(v1);
  
// return the vector to R
#ifdef DEBUG
  SEXP out = PROTECT(allocVector(REALSXP, v1->size));
  for(size_t i = 0; i < v1->size; i++){
    REAL(out)[i] = v1->data[v1->order[i]];
  }
  
  //Rprintf("Destroying Variable\n");
  UNPROTECT(1);
  return out;
#endif
  
  // create an interaction object
  interaction* ivar = interaction_factory(*v1, dy);
  
  release_variable(v1);
  
  return R_NilValue;
  
}