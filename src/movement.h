#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>
#include "utilities.h"

int edge_effect(int pos, int edge_1, int edge_2, int edge_type);

void move(double **pests, double *paras, int ind);

void movement(double **pests, double *paras, double ***land);