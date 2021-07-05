#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

void fill_new_pests(double **pests, double **offspring, double **new_pests,
                    double *paras);