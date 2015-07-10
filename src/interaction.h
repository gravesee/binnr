// functions that combine a variable and performance
#ifndef INTERACTION_H_
#define INTERACTION_H_

#include "variable.h"

typedef struct {
  variable var;
  double* y; // performance
  int* unique_flag; // store flag for unique values
  int num_unique;
  double* unique_vals; // unique values of var.data
  double** agg_counts; // n x 3 matrix of aggregated counts
} interaction;

interaction* interaction_factory(variable v, double* y);

// flag which values are unique
void set_unique_flag(interaction* i);

// using the unique flag, tally all of the values for the aggregation
void aggregate_interaction_counts(interaction* i);

void release_interaction(interaction* ivar);

#endif // INTERACTION_H_