#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

void pest_dense(double **pests, double ***land, double *paras, 
                double *thresholds, double *delay, double *delay_count);