#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

void immigrant_loci_traits(double **new_pests, int row, double *paras);

void fill_new_pests(double **pests, double **offspring, double **new_pests,
                    double *paras, double *imm_sample);