// redo the crossing of two variables here
#ifndef XTAB_SOURCE_FILE_H
#define XTAB_SOURCE_FILE_H

#include "variable.h"

//#define DEBUG

// xtab is an aggregation of unique x and y counts and totals
struct xtab {
  double* ones_ct;
  double* zero_ct;
  double* values;
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

#endif