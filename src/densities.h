#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

void pest_dense(double **pests, double ***land, double *paras, 
                double *thresholds, int *delay, int *delay_count);