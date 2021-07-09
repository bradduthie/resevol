#include "utilities.h"

/* =============================================================================
 * Collects statistics on pest populations
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 * ========================================================================== */
void print_all_pests(double **pests, double *paras, int ts){
  
  int i, j, N, cols, pid;
  
  FILE *ind_output;
  
  N    = (int) paras[101];
  cols = (int) paras[107];

  if(paras[141] == 0){
    if(ts == 0){
        ind_output = fopen("individuals.csv","w");
    }else{
        ind_output = fopen("individuals.csv","a");
    }
    for(i = 0; i < N; i++){
      fprintf(ind_output, "%d,", ts);
      for(j = 0; j < cols; j++){
        fprintf(ind_output, "%f,", pests[i][j]);
      }
      fprintf(ind_output, "\n");
    }
    fclose(ind_output);    
  }
}
