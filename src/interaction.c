#include "interaction.h"
#include "stdlib.h"

// initialize interaction components
interaction* interaction_factory(variable v, double* y) {
  
  interaction* ivar = malloc(sizeof(interaction));
  ivar->var = v;
  ivar->y = y;
  
  // call set_unique_flag
  ivar->unique_flag = malloc(sizeof(ivar->unique_flag) * v.size);
  ivar->unique_vals = NULL;
  set_unique_flag(ivar);
  
  // aggregate over unique values
  ivar->tot_counts = NULL;
  aggregate_interaction_counts(ivar);
  
  // add checks for NULL dependencies?
  aggregate_cumulative_pcts(ivar);
  
  return ivar;
}

// flag which values are unique 
void set_unique_flag(interaction* ivar) {
  variable v = ivar->var;
  int num_unique = 1; // always at least 1 unique value
  
  // loop over sorted var and compare ith element to ith + 1
  ivar->unique_flag[0] = 1;
  for(size_t i = 1; i < v.size; i++) {
    if (v.data[v.order[i-1]] != v.data[v.order[i]]) {
      ivar->unique_flag[i] = 1;
      num_unique++;
    } else {
      ivar->unique_flag[i] = 0;
    }
  }
  
  // set ivar fields and allocate memory for unique vals
  ivar->num_unique = num_unique;
  ivar->unique_vals = malloc(sizeof(ivar->unique_vals) * num_unique);
}

// using the unique flag, tally all of the values for the aggregation
void aggregate_interaction_counts(interaction* ivar) {
  variable v = ivar->var;
  
  // allocate space for array of arrays: 1 row for each unique value
  ivar->agg_counts = malloc(sizeof(size_t*) * (ivar->num_unique));
  ivar->tot_counts = calloc(3, sizeof(size_t));
  
  // loop over data elements and increment at new unique values
  int agg_idx = -1;
  for(size_t i = 0; i < v.size; i++) {
    if (ivar->unique_flag[i] == 1) {
      agg_idx += 1;
      ivar->unique_vals[agg_idx] = v.data[v.order[i]];
      ivar->agg_counts[agg_idx] = calloc(3, sizeof(size_t));
    }
    
    // tally the counts
    if (ivar->y[v.order[i]] == 0) {
      ivar->agg_counts[agg_idx][ZERO_CT]++;
      ivar->tot_counts[ZERO_CT]++;
    } else {
      ivar->agg_counts[agg_idx][ONES_CT]++;
      ivar->tot_counts[ONES_CT]++;
    }
    
    ivar->agg_counts[agg_idx][TOT_CT]++;
    ivar->tot_counts[TOT_CT]++;
  }
  
}

// calculate cum and decum pcts for partitioning
void aggregate_cumulative_pcts(interaction *ivar) {
  Rprintf("In HERE!\n");
  
  size_t** agg_counts = ivar->agg_counts; // local reference to agg_counts
  size_t*  tot_counts = ivar->tot_counts; // ditto tot_counts
  
  ivar->agg_pcts = malloc(sizeof(double*) * (ivar->num_unique));
  
  // loop over agg_counts and calculate stuff
  for(size_t i = 0; i < ivar->num_unique; i++) {
    int idx = ivar->num_unique - i;
    
    ivar->agg_pcts[i] = calloc(4, sizeof(double));
    
    // forwards
    ivar->agg_pcts[i][0] =
      (double) agg_counts[i][ZERO_CT] / tot_counts[ZERO_CT];
    
    ivar->agg_pcts[i][1] =
      (double) agg_counts[i][ONES_CT] / tot_counts[ONES_CT];
    
    // backwards
    ivar->agg_pcts[idx][2] =
      (double) agg_counts[idx][ZERO_CT] / tot_counts[ZERO_CT];
    
    ivar->agg_pcts[idx][3] =
      (double) agg_counts[idx][ONES_CT] / tot_counts[ONES_CT];
    
    Rprintf("Cum Sum 0s: %f\n", ivar->agg_pcts[i][0]);
  }
}


void print_agg_counts(interaction* ivar) {
  // print agg_counts table
  for(size_t i = 0; i < ivar->num_unique; i++) {
    int a, b, c, d;
    a = ivar->unique_vals[i]; // value
    b = ivar->agg_counts[i][ZERO_CT];
    c = ivar->agg_counts[i][ONES_CT];
    d = ivar->agg_counts[i][TOT_CT];
    Rprintf("%5d | %5d | %5d | %5d\n", a, b, c, d);
  }
  Rprintf("TOTAL | %5d | %5d | %5d\n",
          ivar->tot_counts[ZERO_CT],
          ivar->tot_counts[ONES_CT],
          ivar->tot_counts[TOT_CT]);
}

void print_agg_pcts(interaction* ivar) {
  // print agg_counts table
  for(size_t i = 0; i < ivar->num_unique; i++) {
    int a, b, c, d;
    a = ivar->agg_pcts[i][0];
    b = ivar->agg_pcts[i][1];
    c = ivar->agg_pcts[i][2];
    d = ivar->agg_pcts[i][3];
    Rprintf("%5d | %5d | %5d | %5d\n", a, b, c, d);
  }
}

void release_interaction(interaction* ivar) {
  Rprintf("Releasing Resources!\n");
  
  // loop over agg_counts and release agg rows
  for (size_t i = 0; i < ivar->num_unique; i++) {
    free(ivar->agg_counts[i]);
  }
  
  free(ivar->agg_counts);
  free(ivar->tot_counts);
  free(ivar->unique_flag);
  free(ivar->unique_vals);
  free(ivar);
}







