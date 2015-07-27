// redo the crossing of two variables here
#include "variable.h"


// xtab is an aggregation of unique x and y counts and totals
struct xtab {
  double** counts; // counts of 1s and 0s by unique value
  double*  totals; // totals of 1s and 0s overall
  size_t   size;
};

// create and initiazlize the xtab
struct xtab* xtab_factory(struct variable* v, double* y);

// get a flag of unique values for the variable
size_t* create_unique_flag(struct variable* v);

void print_xtab(struct xtab* x);

enum {
  ZERO_CT=1,
  ONES_CT=2,
  VALUE=0,
  TOTS_CT=0
};