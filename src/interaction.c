#include "interaction.h"
#include "stdlib.h"
#include "math.h"

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
  int num_unique = ivar->num_unique;
  
  size_t** agg_counts = ivar->agg_counts; // local reference to agg_counts
  size_t*  tot_counts = ivar->tot_counts; // ditto tot_counts
  
  ivar->woe_table = malloc(sizeof(double*) * (num_unique));
  
  // need to do this as cumulatives and decumulatives
  size_t cuml_zero, cuml_ones, decum_zero, decum_ones;
  double cuml_zero_pct, cuml_ones_pct;
  
  cuml_zero = 0; cuml_ones = 0;;
  
  // loop over agg_counts and calculate stuff
  for(size_t i = 0; i < num_unique; i++) {
    
    ivar->woe_table[i] = malloc(sizeof(double) * 5);
    
    // cumulative pcts
    cuml_zero += agg_counts[i][ZERO_CT];
    cuml_ones += agg_counts[i][ONES_CT];
    
    cuml_zero_pct = (double) cuml_zero / tot_counts[ZERO_CT];
    cuml_ones_pct = (double) cuml_ones / tot_counts[ONES_CT];
    
    
    
    ivar->woe_table[i][WOE] = log(cuml_ones_pct / cuml_zero_pct);
    ivar->woe_table[i][IV]  =
      (cuml_ones_pct - cuml_zero_pct) * ivar->woe_table[i][WOE];
      
    ivar->woe_table[i][WOE_DECUM] =
      log((1 - cuml_ones_pct) / (1 - cuml_zero_pct));
      
    ivar->woe_table[i][IV_DECUM] =
      ((1 - cuml_ones_pct) - (1 - cuml_zero_pct)) * ivar->woe_table[i][WOE_DECUM];
      
    ivar->woe_table[i][IV_SUM] = ivar->woe_table[i][IV] + ivar->woe_table[i][IV_DECUM];
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
    double a, b, c, d, e;
    a = ivar->woe_table[i][WOE];
    b = ivar->woe_table[i][IV];
    c = ivar->woe_table[i][WOE_DECUM];
    d = ivar->woe_table[i][IV_DECUM];
    e = ivar->woe_table[i][IV_SUM];
    Rprintf("%5f | %5f | %5f | %5f | %5f\n", a, b, c, d, e);
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







