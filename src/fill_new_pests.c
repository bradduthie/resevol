#include "utilities.h"

/* =============================================================================
 * Increases the age of each pest by a single time step
 *     pests:           The array holding the parent's information
 *     offspring:       The array that holds the offspring's information
 *     new_pests:       The array that will complete the time step
 *     paras:           The paras vector that holds global information
 * ========================================================================== */
void fill_new_pests(double **pests, double **offspring, double **new_pests,
                    double *paras){
  
  int new_ind, old_ind, N_old, N_offspring, col, cols;
  int mortality_col, off_ind, dead;
  
  mortality_col = (int) paras[81];
  N_old         = (int) paras[101];
  N_offspring   = (int) paras[106];
  cols          = (int) paras[107];
  
  new_ind = 0;
  for(old_ind = 0; old_ind < N_old; old_ind++){
      dead = (int) pests[old_ind][mortality_col];
      if(dead < 1){
          for(col = 0; col < cols; col++){
              new_pests[new_ind][col] = pests[old_ind][col];
          }
          new_ind++;
      }
  }
  
  for(off_ind = 0; off_ind < N_offspring; off_ind++){
      dead = (int) offspring[off_ind][mortality_col];
      if(dead < 1){
          for(col = 0; col < cols; col++){
              new_pests[new_ind][col] = offspring[off_ind][col]; 
          }
          new_ind++;
      }
  }
}
