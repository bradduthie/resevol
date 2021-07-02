#include "utilities.h"

/* =============================================================================
 * Increases the age of each pest by a single time step
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 * ========================================================================== */
void age_pests(double **pests, double *paras){
  
  int ind, N, age_col;
  
  age_col = (int) paras[3];
  N       = (int) paras[51];

  for(ind = 0; ind < N; ind++){
    pests[ind][age_col]++;
  }
  
}
