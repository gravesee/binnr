#include "variable.h"

// Initialized variable structure
variable* initialize_variable (
  double* data,
  int size
) {
  variable* var = malloc(sizeof(variable));
  var->data = data;
  var->size = size;
  return var;
}