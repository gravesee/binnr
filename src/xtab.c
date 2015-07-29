#include "stdlib.h"
#include "xtab.h"
#include "variable.h"
#include "R.h"

// set up a xtab structure that contains: aggregated counts, totals, and size
// will be used for all calculations relating to information value
struct xtab* xtab_factory(struct variable* v, double* y){
  
  struct xtab* xtab = malloc(sizeof(struct xtab*));
  
  size_t* uniq = create_unique_flag(v); // !!! malloc'd -- Needs release
  
  // get number of unique values
  size_t num_unique = 0;
  for (size_t i = 0; i < v->size; i++) {
    num_unique += uniq[i];
  }
  
  // allocate memory for aggregated counts
  double** agg = malloc(sizeof(double*) * num_unique);
  double*  tot = calloc(3, sizeof(double));
  
  size_t idx = -1;
  for (size_t i = 0; i < v->size; i++) {
    // if uniq == 1 then increment the agg index and allocate xtab row memory
    if (uniq[i] == 1) {
        idx += 1;
        agg[idx] = calloc(3, sizeof(double));
        agg[idx][VALUE] = v->data[v->order[i]];
    }
      
    // tally the counts
    if (y[v->order[i]] == 0) {
      agg[idx][ZERO_CT]++;
      tot[ZERO_CT]++;
    } else {
      agg[idx][ONES_CT]++;
      tot[ONES_CT]++;
    }
    
    tot[TOTS_CT]++;
  }
  
  xtab->counts = agg;
  xtab->totals = tot;
  xtab->size = num_unique;
  
  free(uniq);
  
  return(xtab);
}

// create array same size as 'data' where 1 = first uniq value and 0 = non-uniq
size_t* create_unique_flag(struct variable* v) {
  
  size_t* unique_flag = malloc(sizeof(size_t) * v->size);
  
  int num_unique = 1; // always at least 1 unique value
  
  // loop over sorted var and compare ith element to ith + 1
  unique_flag[0] = 1;
  for(size_t i = 1; i < v->size; i++) {
    if (v->data[v->order[i-1]] != v->data[v->order[i]]) {
      unique_flag[i] = 1;
      num_unique++;
    } else {
      unique_flag[i] = 0;
    }
  }
  return unique_flag;
}



// print the aggregated counts in a matrix-like format
void print_xtab(struct xtab* x) {
  double** tmp = x->counts;
  for (size_t i = 0; i < x->size; i++) {
    Rprintf("[%3.0f, %3.0f, %3.0f]\n", tmp[i][VALUE], tmp[i][ZERO_CT], tmp[i][ONES_CT]);
  }
}

// free the resources used by the xtab object
void release_xtab(struct xtab* x) {
  //Rprintf("Releasing the xtab!\n");
  
  // release all of the table pointers
  for (size_t i = 0; i < x->size; i++) {
    free(x->counts[i]);
  }
  free(x->counts);
  free(x->totals);
  free(x);
};