#include "Rinternals.h"
#include "R.h"
#include "stdio.h"
#include "variable.h"
#include "queue.h"
#include "xtab.h"

// prototype for main working function
void find_best_split(struct queue* q, struct xtab* xtab, size_t* breaks, double* grand_tot); 

// called from R and handles passing of data to and from
SEXP bin(SEXP x, SEXP y) {
  
  // grab a few things from the R objects before proceeding
  double* dx = REAL(x);
  double* dy = REAL(y);
  int sz = LENGTH(x);

  // wrap R components in a C structure
  struct variable* v = variable_factory(dx, sz); 
  
// return the vector to R for testing
#ifdef RETURN_R
  SEXP out = PROTECT(allocVector(REALSXP, v->size));
  for(size_t i = 0; i < v->size; i++){
    REAL(out)[i] = v->data[v1->order[i]];
  }
  
  release_variable(v);
  UNPROTECT(1);
  return out;
#endif 
  
  // create the xtab
  struct xtab* xtab = xtab_factory(v, dy);
  
  // create the queue
  struct queue* q = queue_factory();
  
  // add work to the queue
  struct work w = {0, xtab->size - 1}; // last index is one less than the size
  enqueue(q, w);
  
  // bin the variable until it's done
  
  // create a vector to store the split rows and init to zero
  size_t* breaks = calloc(xtab->size, sizeof(size_t));
  double* grand_tots = get_xtab_totals(xtab, 0, xtab->size);
  
  while(!is_empty(q)) {
    find_best_split(q, xtab, breaks, grand_tots);
    // do stuff!
  }
  
  // loop over breaks and print each one out
  Rprintf("Break Values (");
  for (size_t i = 0; i < xtab->size; i++){
    if (breaks[i] == 1) {
      Rprintf(" %4.3f, ",  xtab->counts[i][0]);
    }
  }
  Rprintf(" )");
  
  // Release resources
  release_variable(v);
  release_xtab(xtab);
  release_queue(q);
  free(breaks);
  free(grand_tots);
  
  return R_NilValue;
  
}

void print_calc_table(double** calc, size_t size);

void find_best_split(struct queue* q, struct xtab* xtab, size_t* breaks, double* grand_tot){
  
#ifdef DEBUG
  Rprintf("Splitting new range\n-----------------------------------------\n");
  queue_print(q);
#endif
 
  // take work from queue
  struct work w = dequeue(q);

#ifdef DEBUG
  Rprintf("Working on this range: (%d, %d)\n", w.start, w.stop);
#endif

  // range of values
  size_t range = (w.stop - w.start + 1);

#ifdef DEBUG
  Rprintf("Range value: %d\n", range);
#endif
 
  // find best split within start/stop range
  //double* tot = malloc(sizeof(double) * 2);
  double* tot = get_xtab_totals(xtab, w.start, w.stop + 1);
  
  // calculate column totals
  //for (size_t i = w.start; i <= w.stop; i++) {
  //  tot[0] += xtab->counts[i][ZERO_CT];
  //  tot[1] += xtab->counts[i][ONES_CT];
  //}
  
  
  
  // create data structure to store all calculations required for best split
  // 0) cumulative count zeros
  // 1) cumulative count ones
  // 2) decumulative count zeros
  // 3) decumulative count ones
  // 4) ascending woe
  // 5) ascending iv
  // 6) descending woe
  // 7) descending iv
  // 8) combined iv
  // 9) valid split flag
  double** calc = malloc(sizeof(double*) * range);
  for (size_t i =0; i < range; i++) {
    calc[i] = calloc(10, sizeof(double));
#ifdef DEBUG
    //Rprintf("i = %d\n", i);
#endif
  }
  
  
  double tmp_gds, tmp_bds, cum_gds, cum_bds;
  tmp_gds = tmp_bds = cum_gds = cum_bds = 0;
  
  double best_iv = -1;
  size_t best_split_idx = 0;
  
  // iterate over aggregated counts and fill calc structures
  size_t idx = 0;
  for (size_t i = w.start; i <= w.stop; i++, idx++) {
    // cumulative counts
    cum_gds += xtab->counts[i][ZERO_CT];
    cum_bds += xtab->counts[i][ONES_CT];
    
    calc[idx][0] = cum_gds;
    calc[idx][1] = cum_bds;
    
    // decumulative counts
    calc[idx][2] = tot[0] - calc[idx][0];
    calc[idx][3] = tot[1] - calc[idx][1];
    
    // ascending weight of evidence and information value
    tmp_gds = calc[idx][0]/grand_tot[0];
    tmp_bds = calc[idx][1]/grand_tot[1];
    
    calc[idx][4] = log(tmp_gds / tmp_bds);
    calc[idx][5] = (tmp_gds - tmp_bds) * calc[idx][4];
    
    // descending weight of evidence and information value
    tmp_gds = calc[idx][2]/grand_tot[0];
    tmp_bds = calc[idx][3]/grand_tot[1];
    
    calc[idx][6] = log(tmp_gds / tmp_bds);
    calc[idx][7] = (tmp_gds - tmp_bds) * calc[idx][6];
    
    // combined iv
    calc[idx][8] = calc[idx][5] + calc[idx][7];
    
    // valid split criteria
    
    // minsplit
    if ((calc[idx][0] + calc[idx][1]) < 50) {
      calc[idx][9] = -1;
    } else if ((calc[idx][2] + calc[idx][3]) < 50) {
      calc[idx][9] = -1;
    // min iv
    } else if (calc[idx][8] < 0.01) {
      calc[idx][9] = -1;
    // infinite iv
    } else if (isinf(calc[idx][8])) {
      calc[idx][9] = -1;
    }
    
    if (calc[idx][9] != -1 & calc[idx][8] > best_iv) {
      best_iv = calc[idx][8];
      best_split_idx = idx;
    }
  }
  
  // add two pieces of work to the queue
  if (best_iv > -1) { 
#ifdef DEBUG
    Rprintf("Best split index: %d\n", best_split_idx);
    Rprintf("Best split value: %f\n", xtab->counts[w.start + best_split_idx][0]);
    Rprintf("Work 1 (start, stop): (%d, %d)\n", w.start, w.start + best_split_idx);
    Rprintf("Work 2 (start, stop): (%d, %d)\n", w.start + best_split_idx + 1, w.stop);
#endif

    // update breaks array
    breaks[w.start + best_split_idx] = 1;
    struct work w1 = {w.start, w.start + best_split_idx};
    struct work w2 = {w.start + best_split_idx + 1, w.stop};
    
    // add work to the queue
    enqueue(q, w1);
    enqueue(q, w2);
  } else {
#ifdef DEBUG
    Rprintf("No split found that meets criteria\n", best_split_idx);
#endif
  }
  
  //print_calc_table(calc, idx);
  
  // FREE RESOURCES ALLOCATED IN THIS FUNCTION //
  free(tot);
  
  // destroy the calc object
  for (size_t i = 0; i < range; i++) {
    free(calc[i]);
  }
  free(calc);
}


void print_calc_table(double** calc, size_t size) {
  // 0) cumulative count zeros
  // 1) cumulative count ones
  // 2) decumulative count zeros
  // 3) decumulative count ones
  // 4) ascending woe
  // 5) ascending iv
  // 6) descending woe
  // 7) descending iv
  for (size_t i = 0; i < size; i++) {
    Rprintf("(%3.0f, %3.0f, %3.0f, %3.0f) | (%0.3f, %f, %f, %f | %f)\n",
            calc[i][0],calc[i][1],calc[i][2],calc[i][3],
            calc[i][4],calc[i][5],calc[i][6],calc[i][7], calc[i][8]);
  }
  
}
