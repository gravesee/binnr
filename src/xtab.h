// redo the crossing of two variables here
#include "variable.h"

//#define DEBUG

// xtab is an aggregation of unique x and y counts and totals
struct xtab {
  double** counts; // counts of 1s and 0s by unique value
  //double*  totals; // totals of 1s and 0s overall
  size_t   size;
};

struct split {
  int start;
  size_t size;
};

// create and initiazlize the xtab
struct xtab* xtab_factory(struct variable* v, double* y);

// get a flag of unique values for the variable
size_t* create_unique_flag(struct variable* v);

void print_xtab(struct xtab* x);

void release_xtab(struct xtab* x);

double* get_xtab_totals(struct xtab* xtab, size_t start, size_t stop);

enum {
  VALUE   = 0,
  ZERO_CT = 1,
  ONES_CT = 2
};