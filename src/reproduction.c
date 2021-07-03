#include "utilities.h"

/* =============================================================================
 * Counts the number of offspring that a single individual should produce
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 *     row:             The row of the individual making the offspring
 *     mate_sex:        The identity of the opposite sex that can be a mate
 * ========================================================================== */
int mate_in_range(double **pests, double *paras, int row, int mate_sex){
  
  int ind, N, range_col, range, sex_col;
  int range_count, opp_sex, in_range, selfing, focal_sex;
  int age_col, min_age_col, max_age_col, age, min_age, max_age;
  
  sex_col   = (int) paras[4];
  N         = (int) paras[51];
  range_col = (int) paras[24];
  range     = pests[row][range_col];
  
  age_col         = (int) paras[3];
  min_age_col     = (int) paras[35];
  max_age_col     = (int) paras[36];
  
  focal_sex = (int) pests[ind][sex_col];
  
  range_count = 0;
  for(ind = 0; ind < N; ind++){
    in_range = is_in_range(pests, row, ind, paras, range);
    opp_sex  = (int) pests[ind][sex_col];
    age      = (int) pests[ind][age_col];
    min_age  = (int) pests[ind][min_age_col];
    max_age  = (int) pests[ind][max_age_col];
    if(in_range > 0 && opp_sex == mate_sex && age >= min_age && age <= max_age){
      if(row != ind || selfing > 0){
        range_count++;
      }
    }
  }
  
  return range_count;
}

/* =============================================================================
 * Counts the number of offspring that a single individual should produce
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 *     row:             The row of the individual making the offspring
 * ========================================================================== */
int mate_available(double **pests, double *paras, int row){
  
  int N, ind, sex, sex_col, mate_found,selfing_col, selfing;
  
  N            = (int) paras[51];
  sex_col      = (int) paras[4];
  sex          = (int) pests[row][sex_col];
  selfing_col  = (int) paras[26];
  selfing      = pests[row][selfing_col];
  
  mate_found = 0;
  switch(sex){
      case 0:
          mate_found = 1;
          break;  
      case 1:
          mate_found = mate_in_range(pests, paras, row, 1);
          if(mate_found == 0){ /* Can also look for just a male */
              mate_found = mate_in_range(pests, paras, row, 3);
          }
          break;
      case 2:
          mate_found = mate_in_range(pests, paras, row, 3);
          break;
      case 3:
          mate_found = mate_in_range(pests, paras, row, 2);
          break;
      default:
          break;
  }
  return mate_found;
}

/* =============================================================================
 * Counts the number of offspring that a single individual should produce
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 *     row:             The row of the individual making the offspring
 * ========================================================================== */
void count_offspring(double **pests, double *paras, int row){
  
  int N, repr_param_col, repr_param, offspring, repr_type_col, repr_type;
  int mate_access, mate_access_col, sex_col, sex, off_col;
  int min_age_col, max_age_col, min_age, max_age, age_col, age;
  
  N               = (int) paras[51];
  sex_col         = (int) paras[4];
  sex             = (int) pests[row][sex_col];
  repr_type_col   = (int) paras[23];
  repr_type       = (int) pests[row][repr_type_col];
  repr_param_col  = (int) paras[25];
  repr_param      = pests[row][repr_param_col];
  mate_access_col = (int) paras[27];
  mate_access     = 0;
  off_col         = (int) paras[10];
  age_col         = (int) paras[3];
  min_age_col     = (int) paras[35];
  max_age_col     = (int) paras[36];
  
  age     = (int) pests[row][age_col];
  min_age = (int) pests[row][min_age_col];
  max_age = (int) pests[row][max_age_col];
  
  offspring = 0;
  switch(repr_type){
      case 0:  /* Reproduction is just based off of lambda value */
          mate_access = mate_available(pests, paras, row);
          if(mate_access > 0 && sex < 3 && age >= min_age && age <= max_age){ 
              offspring = rpois(repr_param);
          }
          break;
      case 1: /* Offspring will be based off of food consumed */
          offspring = 1;
          break;
      default:
          mate_access = mate_available(pests, paras, row);
          if(mate_access > 0 && sex < 3 && age >= min_age && age <= max_age){
              offspring = rpois(repr_param);
          }
          break;
  }

  pests[row][mate_access_col] = (double) mate_access;
  if(sex < 3){
    pests[row][off_col] = (double) offspring;
  }

  paras[56] += pests[row][off_col];
}

/* =============================================================================
 * Calculates the number of offspring that each individual should produce
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 * ========================================================================== */
void calculate_offspring(double **pests, double *paras){
  
  int ind, N;

  N = (int) paras[51];
  
  paras[56] = 0.0; /* Start with no offspring */
  
  for(ind = 0; ind < N; ind++){
    count_offspring(pests, paras, ind);
  }
}



