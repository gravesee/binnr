#include "Rinternals.h"
#include "R.h"
#include "stdio.h"
#include "variable.h"
#include "queue.h"
#include "xtab.h"

#define RETURN_R

// prototype for main working function
void find_best_split(
    struct queue* q,
    struct xtab* xtab,
    size_t* breaks,
    double* grand_tot,
    int* num_bins,
    double min_iv,
    int min_cnt,
    int max_bin
  );

// called from R and handles passing of data to and from
SEXP bin(SEXP x, SEXP y, SEXP miniv, SEXP mincnt, SEXP maxbin) {
  
  // grab a few things from the R objects before proceeding
  double* dx = REAL(x);
  double* dy = REAL(y);
  int sz = LENGTH(x);
  
  double min_iv = *REAL(miniv);
  int min_cnt = *INTEGER(mincnt);
  int max_bin = *INTEGER(maxbin);

  // wrap R components in a C structure
  struct variable* v = variable_factory(dx, sz); 
  
  // create the xtab
  struct xtab* xtab = xtab_factory(v, dy);
  
  // create the queue
  struct queue* q = queue_factory();
  
  // add work to the queue
  struct work w = {0, xtab->size - 1}; // last index is one less than the size
  enqueue(q, w);
  
  
  // create a vector to store the split rows and init to zero
  size_t* breaks = calloc(xtab->size, sizeof(size_t));
  double* grand_tots = get_xtab_totals(xtab, 0, xtab->size);
  int num_bins = 1;
  
  // bin the variable until it's done
  while(!is_empty(q)) {
    find_best_split(q, xtab, breaks, grand_tots, &num_bins, min_iv, min_cnt, max_bin);
    // do stuff!
  }

  // return breaks in an R object
#ifdef RETURN_R
  SEXP out = PROTECT(allocVector(REALSXP, num_bins + 1));
  size_t j = 0;
  REAL(out)[0] = R_NegInf;
  for(size_t i = 0; i < xtab->size; i++) {
    if (breaks[i] == 1) {
      j++;
      REAL(out)[j] = xtab->counts[i][0];
    }
    REAL(out)[j + 1] = R_PosInf;
  }
#endif 
  
  // Release resources
  release_variable(v);
  release_xtab(xtab);
  release_queue(q);
  free(breaks);
  free(grand_tots);
  
  
#ifdef RETURN_R 
  UNPROTECT(1);
  return out;
#endif
  
  return R_NilValue;
  
}

void print_calc_table(double** calc, size_t size);

void find_best_split(
  struct queue* q,
  struct xtab* xtab,
  size_t* breaks,
  double* grand_tot,
  int* num_bins,
  double min_iv,
  int min_cnt,
  int max_bin
) {

  // take work from queue
  struct work w = dequeue(q);

  // range of values
  size_t range = (w.stop - w.start + 1);
 
  // find best split within start/stop range
  double* tot = get_xtab_totals(xtab, w.start, w.stop + 1);
  
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
    if ((calc[idx][0] + calc[idx][1]) < min_cnt) {
      calc[idx][9] = -1;
    } else if ((calc[idx][2] + calc[idx][3]) < min_cnt) {
      calc[idx][9] = -1;
    // min iv
    } else if (calc[idx][8] < min_iv) {
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
  
  Rprintf("Best IV: %f\n", best_iv);
  
  // add two pieces of work to the queue
  if (best_iv > -1 & *num_bins < max_bin) {
    (*num_bins)++;
    // update breaks array
    breaks[w.start + best_split_idx] = 1;
    struct work w1 = {w.start, w.start + best_split_idx};
    struct work w2 = {w.start + best_split_idx + 1, w.stop};
    
    // add work to the queue
    enqueue(q, w1);
    enqueue(q, w2);
  }
  
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
