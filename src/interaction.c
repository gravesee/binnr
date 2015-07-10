#include "interaction.h"
#include "stdlib.h"

// initialize interaction components
interaction* interaction_factory(variable v, double* y) {
  
  interaction* ivar = malloc(sizeof(interaction));
  ivar->var = v;
  ivar->y = y;
  
  //Rprintf("Inside interaction factory!\n");
  
  // call set_unique_flag
  ivar->unique_flag = malloc(sizeof(ivar->unique_flag) * v.size);
  set_unique_flag(ivar);
  
  // aggregate over unique values
  aggregate_interaction_counts(ivar);
  
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
  
  ivar->num_unique = num_unique;
}

// using the unique flag, tally all of the values for the aggregation
void aggregate_interaction_counts(interaction* ivar) {
  variable v = ivar->var;
  
  // allocate space for array of arrays: 1 row for each unique value
  ivar->agg_counts = malloc(sizeof(double*) * ivar->num_unique);
  
  // loop over
  int agg_idx = -1;
  for(size_t i = 0; i < v.size; i++) {
    if (ivar->unique_flag[i] == 1) {
      agg_idx += 1;
      ivar->agg_counts[agg_idx] = calloc(4, sizeof(double));
    }
    
    ivar->agg_counts[agg_idx][0] = v.data[v.order[i]];
    ivar->agg_counts[agg_idx][1] += ivar->y[v.order[i]] == 0 ? 1 : 0;
    ivar->agg_counts[agg_idx][2] += ivar->y[v.order[i]] == 1 ? 1 : 0;
    ivar->agg_counts[agg_idx][3]++;
    
//    Rprintf("Y values: %d\n", ivar->y[v.order[i]]);
    
  }
  
  // print agg_counts table
  
  for(size_t i = 0; i < ivar->num_unique; i++) {
    int a, b, c, d;
    a = ivar->agg_counts[i][0];
    b = ivar->agg_counts[i][1];
    c = ivar->agg_counts[i][2];
    d = ivar->agg_counts[i][3];
    
    Rprintf("%5d | %5d | %5d | %5d\n", a, b, c, d);
  }

};





