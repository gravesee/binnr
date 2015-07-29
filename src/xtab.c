#include "stdlib.h"
#include "xtab.h"
#include "variable.h"
#include "R.h"

// set up a xtab structure that contains: aggregated counts, totals, and size
// will be used for all calculations relating to information value
struct xtab* xtab_factory(struct variable* v, double* y){
  
  struct xtab* xtab = malloc(sizeof(*xtab));
  
  size_t* uniq = create_unique_flag(v); // !!! malloc'd -- Needs release
  
  // get number of unique values
  size_t num_unique = 0;
  for (size_t i = 0; i < v->size; i++) {
    num_unique += uniq[i];
  }
  
  // allocate memory for aggregated counts
#ifdef DEBUG
  Rprintf("Number of Unique Values: %d\n", num_unique);
#endif

  double** agg = malloc(sizeof(double*) * num_unique);
  //double*  tot = calloc(3, sizeof(double));
  
  size_t idx = -1;
  for (size_t i = 0; i < v->size; i++) {
    // if uniq == 1 then increment the agg index and allocate xtab row memory
    if (uniq[i] == 1) {
        idx += 1;
#ifdef DEBUG
        //Rprintf("Agg index: %d\n", idx);
#endif
        agg[idx] = calloc(3, sizeof(double));
        agg[idx][VALUE] = v->data[v->order[i]];
    }
      
    // tally the counts
    //Rprintf("Index: %d | Num_Unique: %d\n", idx, num_unique);
    if (y[v->order[i]] == 0) {
      agg[idx][ZERO_CT]++;
      //tot[ZERO_CT]++;
    } else {
      agg[idx][ONES_CT]++;
      //tot[ONES_CT]++;
    }
    
    //tot[TOTS_CT]++;
  }
  
  xtab->counts = agg;
  //xtab->totals = tot;
  xtab->size = num_unique;
  
  print_xtab(xtab);
  
  free(uniq);
  
  return(xtab);
}

// create array same size as 'data' where 1 = first uniq value and 0 = non-uniq
size_t* create_unique_flag(struct variable* v) {
  
  size_t* unique_flag = malloc(sizeof(size_t) * v->size);
  
  //Rprintf("Size of variable: %d\n", v->size);
  //int num_unique = 1; // always at least 1 unique value
  
  // loop over sorted var and compare ith element to ith + 1
  unique_flag[0] = 1;
  for(size_t i = 1; i < v->size; i++) {
    if (v->data[v->order[i-1]] != v->data[v->order[i]]) {
      unique_flag[i] = 1;
      // num_unique++;
    } else {
      unique_flag[i] = 0;
    }
    //Rprintf("Unique Flag: %d\n", unique_flag[i]);
  }
  //Rprintf("Size of variable: %d\n", v->size);
  return unique_flag;
}

// return pointer to int array of length 2 with 0s and 1s totals
double* get_xtab_totals(struct xtab* xtab, size_t start, size_t stop) {
#ifdef DEBUG
  Rprintf("Getting xtab totals\n");
#endif
  double* tot = calloc(2, sizeof(double));
  
  // calculate column totals
  for (size_t i = start; i < stop; i++) {
    tot[0] += xtab->counts[i][ZERO_CT];
    tot[1] += xtab->counts[i][ONES_CT];
  }
#ifdef DEBUG
  Rprintf("Totals are: (%3.0f, %3.0f)\n", tot[0], tot[1]);
#endif
  return tot;
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
  //free(x->totals);
  free(x);
};