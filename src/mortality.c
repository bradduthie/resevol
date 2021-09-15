#include "utilities.h"

/* =============================================================================
 * Mortality is or is not inflicted on the individual
 *     pests: The array holding the parent's information
 *     paras: The paras vector that holds global information
 *     ind:   The individual of focus that will be eating        
 * ========================================================================== */
void mortality(double **pests, double *paras, int ind){
  
  int age_col, mortality_type_col, mortality_type, mortality_col, max_age;
  int food_threshold_age_col, pesticide_threshold_age_col, max_age_col;
  int pesticide_consumed_col, food_age, pesticide_age, food_consumed_col, age;
  int food_threshold_col, pesticide_threshold_col;
  double food_consumed, pesticide_consumed, food_threshold, pesticide_threshold;
  
  age_col                       = (int) paras[3];
  food_consumed_col             = (int) paras[14];
  pesticide_consumed_col        = (int) paras[15];
  food_threshold_col            = (int) paras[16];
  pesticide_threshold_col       = (int) paras[17];
  mortality_type_col            = (int) paras[79];
  max_age_col                   = (int) paras[80];
  mortality_col                 = (int) paras[81];
  food_threshold_age_col        = (int) paras[82];
  pesticide_threshold_age_col   = (int) paras[83];
  
  mortality_type      = (int) pests[ind][mortality_type_col];
  age                 = (int) pests[ind][age_col];
  max_age             = (int) pests[ind][max_age_col];
  food_age            = (int) pests[ind][food_threshold_age_col];
  pesticide_age       = (int) pests[ind][pesticide_threshold_age_col];
  food_consumed       = pests[ind][food_consumed_col];
  pesticide_consumed  = pests[ind][pesticide_consumed_col];
  food_threshold      = pests[ind][food_threshold_col];
  pesticide_threshold = pests[ind][pesticide_threshold_col];
  
  switch(mortality_type){
      case 0: /* Mortality if too old, not enough food, too much pesticide */
          if(age >= max_age){
              pests[ind][mortality_col] = 1.0;
          }
          if(food_consumed < food_threshold && age >= food_age){
              pests[ind][mortality_col] = 1.0;
          }
          if(pesticide_consumed > pesticide_threshold && age >= pesticide_age){
              pests[ind][mortality_col] = 1.0;
          }
          break;
      default: /* Mortality if too old, not enough food, too much pesticide */
          if(age >= max_age){
              pests[ind][mortality_col] = 1.0;
          }
          if(food_consumed < food_threshold && age >= food_age){
              pests[ind][mortality_col] = 1.0;
          }
          if(pesticide_consumed > pesticide_threshold && age >= pesticide_age){
              pests[ind][mortality_col] = 1.0;
          }
          break;
  }

  if(pests[ind][mortality_col] > 0){
      paras[138]--;
  }
}

/* =============================================================================
 * Cycle throught mortality function for all individuals
 *     pests: The array holding the parent's information
 *     paras: The paras vector that holds global information
 * ========================================================================== */
void apply_mortality(double **pests, double *paras){
  
  int ind, N;
  
  N          = (int) paras[101];
  paras[138] = (double) N;
  
  for(ind = 0; ind < N; ind++){
      mortality(pests, paras, ind);
  }
}

