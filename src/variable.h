#include "Rinternals.h"
#include "R.h"

typedef struct {
  double* data;
  int size;
} variable;

variable* initialize_variable(
  double* data,
  int size
);