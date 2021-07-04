#include "utilities.h"

/* =============================================================================
 * Moves a single individual on the x-y location where it is located
 *     pests: The array holding the parent's information
 *     paras: The paras vector that holds global information
 *     land:  The landscape array with food on it
 *     ind:   The individual of focus that will be moving        
 * ========================================================================== */
void move(double **pests, double *paras, double ***land, int ind){
  
  int N, age_col;
  
  age_col = (int) paras[3];
  N       = (int) paras[101];

  for(ind = 0; ind < N; ind++){
    pests[ind][age_col]++;
  }
  
}

/*
 * 
 * Going to need to include the torus here, as we as checking for movement type,
 * movement bouts, and movement distance
 * 
 */