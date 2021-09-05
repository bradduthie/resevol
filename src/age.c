#include "utilities.h"

/* =============================================================================
 * Increases the age of each pest by a single time step
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 * ========================================================================== */
void age_pests(double **pests, double *paras){
  
  int ind, N, age_col, meta_col, cons_col, base_m_col, min_age_col, max_age_col;
  int age, min_age, max_age;
  double meta_rate, base_metabolism, metabolism;
  
  age_col     = (int) paras[3];
  N           = (int) paras[101];
  meta_col    = (int) paras[86];
  base_m_col  = (int) paras[87];
  cons_col    = (int) paras[14];
  min_age_col = (int) paras[88];
  max_age_col = (int) paras[89];

  for(ind = 0; ind < N; ind++){
      age     = pests[ind][age_col];
      min_age = pests[ind][min_age_col];
      max_age = pests[ind][max_age_col];
    
      if(age >= min_age && age <= max_age){
          base_metabolism = pests[ind][base_m_col];
          metabolism      = pests[ind][meta_col];
          meta_rate       = base_metabolism + metabolism;
          if(meta_rate < 0){
              meta_rate = 0.0;
          }
          pests[ind][cons_col] -= meta_rate;
          if(pests[ind][cons_col] < 0){
              pests[ind][cons_col] = 0;
          }
      }
      pests[ind][age_col]++;
  }
}
