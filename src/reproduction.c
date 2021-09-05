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
  N         = (int) paras[101];
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
  
  N            = (int) paras[101];
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
          /* Below is unlikely to ever be needed, so avoid for now 
          if(mate_found == 0){ 
              mate_found = mate_in_range(pests, paras, row, 3);
          }
          */
          break;
      case 2:
          mate_found = mate_in_range(pests, paras, row, 3);
          break;
      case 3:
          /* Only use if really need to know male's mate accessibility 
          mate_found = mate_in_range(pests, paras, row, 2);
          */
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
  
  int N, repr_param_col, offspring, repr_type_col, repr_type;
  int mate_access, mate_access_col, off_col;
  int food_consumed_col, food_needed_col, repr_incr_col;
  double food_consumed, food_needed, repr_incr, repr_param;
  
  N                  = (int) paras[101];
  repr_type_col      = (int) paras[23];
  repr_type          = (int) pests[row][repr_type_col];
  repr_param_col     = (int) paras[25];
  repr_incr_col      = (int) paras[85];
  repr_param         = pests[row][repr_param_col];
  repr_incr          = pests[row][repr_incr_col];
  mate_access_col    = (int) paras[27];
  mate_access        = 0;
  off_col            = (int) paras[10];
  food_consumed_col  = (int) paras[14];
  food_needed_col    = (int) paras[18];
  
  offspring = 0;
  switch(repr_type){
      case 0:  /* Reproduction is just based off of lambda value */
          mate_access = mate_available(pests, paras, row);
          if(mate_access > 0){
              offspring = 0;
              if( (repr_param + repr_incr) > 0 ){
                  offspring = rpois(repr_param + repr_incr);
              }
          }
          break;
      case 1: /* Offspring will be based off of food consumed */
          mate_access = mate_available(pests, paras, row);
          if(mate_access > 0){ 
              food_consumed = pests[row][food_consumed_col];
              food_needed   = pests[row][food_needed_col];
              offspring     = (int) floor(food_consumed / food_needed);
          }
          break;
      default:
          mate_access = mate_available(pests, paras, row);
          if(mate_access > 0){
              offspring = 0;
              if( (repr_param + repr_incr) > 0 ){
                  offspring = rpois(repr_param + repr_incr);
              }
          }
          break;
  }

  pests[row][mate_access_col] = (double) mate_access;
  pests[row][off_col]         = (double) offspring;

  paras[106] += pests[row][off_col];
}

/* =============================================================================
 * Calculates the number of offspring that each individual should produce
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 * ========================================================================== */
void calculate_offspring(double **pests, double *paras){
  
  int ind, N, tot_offspring, birth_K, off_col, sex_col, sex, pest_thresh_col;
  int age_col, age, min_age_col, max_age_col, min_age, max_age, pest_consum_col;
  double pesticide_thresh, pesticide_consumed;

  age_col         = (int) paras[3];
  sex_col         = (int) paras[4];
  off_col         = (int) paras[10];
  pest_consum_col = (int) paras[15];
  pest_thresh_col = (int) paras[19];
  min_age_col     = (int) paras[35];
  max_age_col     = (int) paras[36];
  N               = (int) paras[101];
  birth_K         = (int) paras[167];
  
  paras[106]         = 0.0; /* Start with no offspring */
  
  for(ind = 0; ind < N; ind++){
      sex                = (int) pests[ind][sex_col];
      age                = (int) pests[ind][age_col];
      min_age            = (int) pests[ind][min_age_col];
      max_age            = (int) pests[ind][max_age_col];
      pesticide_thresh   = pests[ind][pest_thresh_col];
      pesticide_consumed = pests[ind][pest_consum_col];
      if(age >= min_age && age <= max_age && sex < 3 && 
         pesticide_consumed < pesticide_thresh){
          count_offspring(pests, paras, ind);
      }
  }
  
  tot_offspring = (int) paras[106];
  if(birth_K > 0){
      while(tot_offspring > birth_K){
          ind = get_rand_int(0, N - 1);
          if(pests[ind][off_col] > 0){
              pests[ind][off_col]--;
              tot_offspring--;
          }
      }
      paras[106] = (double) tot_offspring;
  }
}



