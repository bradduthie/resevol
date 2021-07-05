#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

void mortality(double **pests, double *paras, int ind);

void apply_mortality(double **pests, double *paras);