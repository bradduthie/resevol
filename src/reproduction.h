#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

void calculate_offspring(double **pests, double *paras);

void count_offspring(double **pests, double *paras, int row);

int mate_available(double **pests, double *paras, int row);

int mate_in_range(double **pests, double *paras, int row, int mate_sex);