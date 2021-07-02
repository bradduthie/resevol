#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

void matrix_multiply(double **m1, double **m2, int m1_rows, int m1_cols,
                     int m2_rows, int m2_cols, double **m_out);

void sum_network_layers(int traits, int layers, double ***net, 
                        double **net_out);

void matrix_zeros(int rows, int cols, double **mat);

int is_in_range(double **pests, int focal, int other, double *paras, int range);

int get_rand_int(int from, int to);

void swap_arrays(void **ARRAY_A, void **ARRAY_B);

int max_in_col(double **array, int rows, int col);
