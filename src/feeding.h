#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

void feed(double **pests, double *paras, double ***land, int ind);

void refresh_consumed(double **pests, double *paras);

void feeding(double **pests, double *paras, double ***land);