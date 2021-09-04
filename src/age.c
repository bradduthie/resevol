#include "utilities.h"

/* =============================================================================
 * Increases the age of each pest by a single time step
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 * ========================================================================== */
void age_pests(double **pests, double *paras){
  
  int ind, N, age_col, meta_col, cons_col;
  
  age_col  = (int) paras[3];
  N        = (int) paras[101];
  meta_col = (int) paras[86];
  cons_col = (int) paras[14];

  for(ind = 0; ind < N; ind++){
    if(pests[ind][age_col] > 0){
        pests[ind][cons_col] -= pests[ind][meta_col];
        if(pests[ind][cons_col] < 0){
            pests[ind][cons_col] = 0;
        }
    }
    pests[ind][age_col]++;
  }
  
}
