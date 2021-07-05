#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

void pesticide(double **pests, double *paras, double ***land, int ind);

void pesticide_consumed(double **pests, double *paras, double ***land);