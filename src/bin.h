#ifndef BIN_H
#define BIN_H

#include "R.h"
#include "variable.h"
#include "queue.h"
#include "xtab.h"

#define RETURN_R

struct iv {
  double asc_woe;
  double dsc_woe;
  double iv;
};

struct opts {
  double min_iv;
  int min_cnt;
  int max_bin;
  int mono;
};

// prototype for main working function
size_t find_best_split(int start, int stop, struct xtab* xtab, double* grand_tot, struct opts opts);

SEXP bin(SEXP x, SEXP y, SEXP miniv, SEXP mincnt, SEXP maxbin, SEXP monotonicity);

struct iv calc_iv(double* asc_cnts, double* dsc_cnts, double* tots);

double calc_sv_woe(double* dx, double* dy, int size, double value, double* tots);

#endif