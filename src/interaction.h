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
  
  size_t** agg_counts; // n x 3 matrix of aggregated counts
  size_t* tot_counts; // table totals
  
  double** woe_table; // n x 2 matrix containing all info needed for partitioning
  
} interaction;

enum AGG_POS {
  ONES_CT, ZERO_CT, TOT_CT
};

enum PCT_POS {
  WOE, IV, WOE_DECUM, IV_DECUM, IV_SUM
};

interaction* interaction_factory(variable v, double* y);

// flag which values are unique
void set_unique_flag(interaction* ivar);

// using the unique flag, tally all of the values for the aggregation
void aggregate_interaction_counts(interaction* ivar);

void aggregate_cumulative_pcts(interaction* ivar);

void print_agg_counts(interaction* ivar);

void print_agg_pcts(interaction* ivar);

void release_interaction(interaction* ivar);

int find_best_split(interaction* ivar); // return index of best split

#endif // INTERACTION_H_