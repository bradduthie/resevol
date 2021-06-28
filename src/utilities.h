#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

int get_rand_int(int from, int to);

void swap_arrays(void **ARRAY_A, void **ARRAY_B);

int max_in_col(double **array, int rows, int col);


int is_in_range(double **pests, int focal, int other, double *paras, int range);