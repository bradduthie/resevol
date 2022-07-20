#include "utilities.h"

/* =============================================================================
 * Prints individuals in the pest population
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 * ========================================================================== */
void print_all_pests(double **pests, double *paras, int ts){
  
  int i, j, N, cols, extinct, print_inds, print_last, last_time;
  
  FILE *ind_output;
  
  N          = (int) paras[101];
  cols       = (int) paras[107];
  last_time  = (int) paras[140];
  extinct    = (int) paras[141];
  print_inds = (int) paras[164];
  print_last = (int) paras[166];
  
  if(extinct == 0 && print_inds > 0){
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
  
  if(print_last > 0 && ts == last_time - 1){
    ind_output = fopen("last_time_step.csv","w");
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


/* =============================================================================
 * Prints population level statistics in the pest population
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 * ========================================================================== */
void population_statistics(double **pests, double *paras, int ts){
  
  int i, j, N, age_col, sex_col, traits_col, traits;  
  int food_consumed_col, pesticide_consumed_col, mated_col, trait_start_col;
  int food1_col, food2_col, food3_col, food4_col, food5_col, food6_col; 
  int food7_col, food8_col, food9_col, food10_col;
  int pesticide1_col, pesticide2_col, pesticide3_col, pesticide4_col;
  int pesticide5_col, pesticide6_col, pesticide7_col, pesticide8_col;
  int pesticide9_col, pesticide10_col, mortality_col;
  int food_types_used, pesticide_types_used, f_col;
  double NN, m_age, m_sex, m_food_consumed, m_pesticide_consumed, m_mated;
  double m_mortality, *m_traits, *m_food_vals, *m_pesticide_vals, m_fval;
  
  FILE *pop_output;
  
  food_types_used        = (int) paras[156];
  pesticide_types_used   = (int) paras[157];
  
  N                      = (int) paras[101];
  age_col                = (int) paras[3];
  sex_col                = (int) paras[4]; 
  traits_col             = (int) paras[12];
  food_consumed_col      = (int) paras[14];
  pesticide_consumed_col = (int) paras[15];
  mated_col              = (int) paras[27];
  food1_col              = (int) paras[58];
  food2_col              = (int) paras[59];
  food3_col              = (int) paras[60];
  food4_col              = (int) paras[61];
  food5_col              = (int) paras[62];
  food6_col              = (int) paras[63];
  food7_col              = (int) paras[64];
  food8_col              = (int) paras[65];
  food9_col              = (int) paras[66];
  food10_col             = (int) paras[67];
  pesticide1_col         = (int) paras[68];
  pesticide2_col         = (int) paras[69];
  pesticide3_col         = (int) paras[70];
  pesticide4_col         = (int) paras[71];
  pesticide5_col         = (int) paras[72];
  pesticide6_col         = (int) paras[73];
  pesticide7_col         = (int) paras[74];
  pesticide8_col         = (int) paras[75];
  pesticide9_col         = (int) paras[76];
  pesticide10_col        = (int) paras[77];
  mortality_col          = (int) paras[81];
  f_col                  = (int) paras[84];
  trait_start_col        = (int) paras[109];
  
  traits           = (int) pests[0][traits_col];
  m_traits         = (double *) malloc(traits * sizeof(double));
  m_food_vals      = (double *) malloc(10 * sizeof(double));
  m_pesticide_vals = (double *) malloc(10 * sizeof(double));
  NN               = (double) N;
  
  m_age                  = 0.0;
  m_sex                  = 0.0; 
  m_food_consumed        = 0.0;
  m_pesticide_consumed   = 0.0;
  m_mated                = 0.0;
  m_mortality            = 0.0;
  for(j = 0; j < traits; j++){
      m_traits[j] = 0.0;
  }
  for(j = 0; j < 10; j++){
      m_food_vals[j]      = 0.0;
      m_pesticide_vals[j] = 0.0;
  }
  m_fval = 0.0;
  
  for(i = 0; i < N; i++){
      m_age                 += pests[i][age_col];
      m_sex                 += pests[i][sex_col];
      m_food_consumed       += pests[i][food_consumed_col];
      m_pesticide_consumed  += pests[i][pesticide_consumed_col];
      m_mated               += pests[i][mated_col];
      m_food_vals[0]        += pests[i][food1_col];
      m_food_vals[1]        += pests[i][food2_col];
      m_food_vals[2]        += pests[i][food3_col];
      m_food_vals[3]        += pests[i][food4_col];
      m_food_vals[4]        += pests[i][food5_col];
      m_food_vals[5]        += pests[i][food6_col];
      m_food_vals[6]        += pests[i][food7_col];
      m_food_vals[7]        += pests[i][food8_col];
      m_food_vals[8]        += pests[i][food9_col];
      m_food_vals[9]        += pests[i][food10_col];
      m_pesticide_vals[0]   += pests[i][pesticide1_col];
      m_pesticide_vals[1]   += pests[i][pesticide2_col];
      m_pesticide_vals[2]   += pests[i][pesticide3_col];
      m_pesticide_vals[3]   += pests[i][pesticide4_col];
      m_pesticide_vals[4]   += pests[i][pesticide5_col];
      m_pesticide_vals[5]   += pests[i][pesticide6_col];
      m_pesticide_vals[6]   += pests[i][pesticide7_col];
      m_pesticide_vals[7]   += pests[i][pesticide8_col];
      m_pesticide_vals[8]   += pests[i][pesticide9_col];
      m_pesticide_vals[9]   += pests[i][pesticide10_col];
      m_mortality           += pests[i][mortality_col];
      for(j = 0; j < traits; j++){
          m_traits[j] += pests[i][trait_start_col + j];
      }
      m_fval += pests[i][f_col];
  }
  
  m_age                 *= 1/NN;
  m_sex                 *= 1/NN;
  m_food_consumed       *= 1/NN;
  m_pesticide_consumed  *= 1/NN;
  m_mated               *= 1/NN;
  m_mortality           *= 1/NN;
  for(j = 0; j < 10; j++){
      m_food_vals[j]      *= 1/NN;
      m_pesticide_vals[j] *= 1/NN;
  }
  
  for(j = 0; j < traits; j++){
      m_traits[j] *= 1/NN;
  }
  m_fval *= 1/NN;
  
  if(ts == 0){
      pop_output = fopen("population_data.csv","w");
      fprintf(pop_output, "time_step,\
                           population_size,\
                           mean_age,\
                           mean_sex,\
                           mean_food_consumed,\
                           mean_pesticide_consumed,\
                           mortality_rate,");
      for(i = 0; i < food_types_used; i++){
          fprintf(pop_output, "mean_food%d_consumed,", i + 1);
      }
      for(i = 0; i < pesticide_types_used; i++){
          fprintf(pop_output, "mean_pesticide%d_consumed,", i + 1);
      }
      for(i = 0; i < traits; i++){
          fprintf(pop_output, "trait%d_mean_value,", i + 1);
      }
      fprintf(pop_output, "mean_f");
      fprintf(pop_output, "\n");
      fclose(pop_output);
  }
  
  pop_output = fopen("population_data.csv","a");
  fprintf(pop_output, "%d, %d, %f, %f, %f, %f, %f,", ts, N, m_age, m_sex, 
          m_food_consumed, m_pesticide_consumed, m_mortality);
  for(i = 0; i < food_types_used; i++){
      fprintf(pop_output, "%f,", m_food_vals[i]);
  }
  for(i = 0; i < pesticide_types_used; i++){
      fprintf(pop_output, "%f,", m_pesticide_vals[i]);
  }
  for(i = 0; i < traits; i++){
      fprintf(pop_output, "%f,", m_traits[i]);
  }
  fprintf(pop_output, "%f", m_fval);
  fprintf(pop_output, "\n");
  fclose(pop_output);
  
  free(m_traits);
  free(m_food_vals);
  free(m_pesticide_vals);
  
}

