#include <time.h>
#include "utilities.h"

/* =============================================================================
 * Gets an actual value for the crop being inserted into the landscape
 *     paras:  The paras vector that holds global information
 * ========================================================================== */
double get_pesticide_val(double *paras){
  
  double pesticide_amount, pesticide_sd, pesticide_min, pesticide_max, val;
  
  pesticide_amount = paras[150];
  pesticide_min    = paras[153];
  pesticide_max    = paras[154];
  pesticide_sd     = paras[152];
  
  if(pesticide_sd == 0){
      val = pesticide_amount;
  }else{
      val = rnorm(pesticide_amount, pesticide_sd);
  }
  
  if(val < pesticide_min){
      val = pesticide_min;
  }
  if(val > pesticide_max){
      val = pesticide_max;
  }
  
  return val;
}

/* =============================================================================
 * Initialise pesticides in their starting positions on the landscape
 *     land:   The landscape array to be adjusted
 *     paras:  The paras vector that holds global information
 *     P_init: The matrix for the initial pesticide positions
 * ========================================================================== */
void init_pesticide(double ***land, double *paras, double **P_init){
    
    int i, j, xdim, ydim, owner, own_layer, choice, layer, farms;
    int pesticide_number, pesticide_layer_1, *owner_choice;
    double *init_vec;
    
    xdim              = (int) paras[103];
    ydim              = (int) paras[104];
    farms             = (int) paras[142];
    own_layer         = (int) paras[155];
    pesticide_layer_1 = (int) paras[128];
    own_layer         = (int) paras[155];
    pesticide_number  = (int) paras[157];
    
    owner_choice = (int *) malloc(farms * sizeof(int));
    for(i = 0; i < farms; i++){
        init_vec = (double *) malloc(pesticide_number * sizeof(double));
        for(j = 0; j < pesticide_number; j++){
            init_vec[j] = P_init[i][j];
        }
        owner_choice[i] = sample_pr_vector(init_vec, pesticide_number);
        free(init_vec);
    }
    
    for(i = 0; i < xdim; i++){
        for(j = 0; j < ydim; j++){
            owner             = (int) land[i][j][own_layer] - 1;
            choice            = (int) owner_choice[owner];
            layer             = choice + pesticide_layer_1;
            land[i][j][layer] = get_pesticide_val(paras);
        }
    }
    
    free(owner_choice);
}

/* =============================================================================
 * Change the pesticide choice based on the rotation matrix
 *     P_init:   The matrix for the initial (current) pesticide positions
 *     P_change: The matrix describing how pesticide changes occur
 *     paras:    The paras vector that holds global information
 * ========================================================================== */
void change_pesticide_choice(double **P_init, double **P_change, double *paras){
    
    int i, j, farms, pesticide_number, now_choice, new_choice;
    
    farms             = (int) paras[142];
    pesticide_number  = (int) paras[157];
    
    for(i = 0; i < farms; i++){
        now_choice = sample_pr_vector(P_init[i], pesticide_number);
        new_choice = sample_pr_vector(P_change[now_choice], pesticide_number);
        for(j = 0; j < pesticide_number; j++){
            P_init[i][j] = 0.0;
        }
        P_init[i][new_choice] = 1.0;
    }
}

/* =============================================================================
 * Gets an actual value for the crop being inserted into the landscape
 *     paras:  The paras vector that holds global information
 * ========================================================================== */
double get_crop_val(double *paras){

    double crop_production, crop_prod_sd, crop_prod_min, crop_prod_max, val;
  
    crop_production = paras[144];
    crop_prod_min   = paras[146];
    crop_prod_max   = paras[147];
    crop_prod_sd    = paras[162];
    
    if(crop_prod_sd == 0){
        val = crop_production;
    }else{
        val = rnorm(crop_production, crop_prod_sd);
    }
    
    if(val < crop_prod_min){
        val = crop_prod_min;
    }
    if(val > crop_prod_max){
        val = crop_prod_max;
    }
  
    return val;
}

/* =============================================================================
 * Cleans the landscape of all crops
 *     land:   The landscape array to be adjusted
 *     paras:  The paras vector that holds global information
 * ========================================================================== */
void clean_pesticide(double ***land, double *paras){
  
  int i, j, xdim, ydim;
  int pest1_col, pest2_col, pest3_col, pest4_col, pest5_col, pest6_col;
  int pest7_col, pest8_col, pest9_col, pest10_col;
  
  xdim       = (int) paras[103];
  ydim       = (int) paras[104];
  pest1_col  = (int) paras[128];
  pest2_col  = (int) paras[129];
  pest3_col  = (int) paras[130];
  pest4_col  = (int) paras[131];
  pest5_col  = (int) paras[132];
  pest6_col  = (int) paras[133];
  pest7_col  = (int) paras[134];
  pest8_col  = (int) paras[135];
  pest9_col  = (int) paras[136];
  pest10_col = (int) paras[137];
  
  for(i = 0; i < xdim; i++){
    for(j = 0; j < ydim; j++){
      land[i][j][pest1_col]  = 0.0;
      land[i][j][pest2_col]  = 0.0;
      land[i][j][pest3_col]  = 0.0;
      land[i][j][pest4_col]  = 0.0;
      land[i][j][pest5_col]  = 0.0;
      land[i][j][pest6_col]  = 0.0;
      land[i][j][pest7_col]  = 0.0;
      land[i][j][pest8_col]  = 0.0;
      land[i][j][pest9_col]  = 0.0;
      land[i][j][pest10_col] = 0.0;
    }
  }
}

/* =============================================================================
 * Cleans the landscape of all crops
 *     land:   The landscape array to be adjusted
 *     paras:  The paras vector that holds global information
 * ========================================================================== */
void clean_crops(double ***land, double *paras){
  
  int i, j, xdim, ydim;
  int crop1_col, crop2_col, crop3_col, crop4_col, crop5_col, crop6_col;
  int crop7_col, crop8_col, crop9_col, crop10_col;
  
  xdim       = (int) paras[103];
  ydim       = (int) paras[104];
  crop1_col  = (int) paras[118];
  crop2_col  = (int) paras[119];
  crop3_col  = (int) paras[120];
  crop4_col  = (int) paras[121];
  crop5_col  = (int) paras[122];
  crop6_col  = (int) paras[123];
  crop7_col  = (int) paras[124];
  crop8_col  = (int) paras[125];
  crop9_col  = (int) paras[126];
  crop10_col = (int) paras[127];
  
  for(i = 0; i < xdim; i++){
      for(j = 0; j < ydim; j++){
          land[i][j][crop1_col]  = 0.0;
          land[i][j][crop2_col]  = 0.0;
          land[i][j][crop3_col]  = 0.0;
          land[i][j][crop4_col]  = 0.0;
          land[i][j][crop5_col]  = 0.0;
          land[i][j][crop6_col]  = 0.0;
          land[i][j][crop7_col]  = 0.0;
          land[i][j][crop8_col]  = 0.0;
          land[i][j][crop9_col]  = 0.0;
          land[i][j][crop10_col] = 0.0;
      }
  }
}

/* =============================================================================
 * Cleans the landscape of all but the owner layer
 *     land:   The landscape array to be adjusted
 *     paras:  The paras vector that holds global information
 * ========================================================================== */
void clean_landscape(double ***land, double *paras){
  
  int i, j, k, xdim, ydim, layers, own_layer;
  
  xdim      = (int) paras[103];
  ydim      = (int) paras[104];
  layers    = (int) paras[105];
  own_layer = (int) paras[155];
  
  for(k = 0; k < layers; k++){
      for(i = 0; i < xdim; i++){
          for(j = 0; j < ydim; j++){
              if(k != own_layer){
                  land[i][j][k] = 0;
              }
          }
      }
  }
}

 /* =============================================================================
  * Initialise crops in their starting positions on the landscape
  *     land:   The landscape array to be adjusted
  *     paras:  The paras vector that holds global information
  *     C_init: The matrix for the initial crop positions
  * ========================================================================== */
 void init_crop(double ***land, double *paras, double **C_init){
     
     int i, j, xdim, ydim, owner, own_layer, choice, layer, farms;
     int crop_number, food_layer_1, *owner_choice;
     double *init_vec;
     
     xdim         = (int) paras[103];
     ydim         = (int) paras[104];
     food_layer_1 = (int) paras[118];
     farms        = (int) paras[142];
     own_layer    = (int) paras[155];
     crop_number  = (int) paras[156];
     
     owner_choice = (int *) malloc(farms * sizeof(int));
     for(i = 0; i < farms; i++){
         init_vec = (double *) malloc(crop_number * sizeof(double));
         for(j = 0; j < crop_number; j++){
             init_vec[j] = C_init[i][j];
         }
         owner_choice[i] = sample_pr_vector(init_vec, crop_number);
         free(init_vec);
     }
     
     for(i = 0; i < xdim; i++){
         for(j = 0; j < ydim; j++){
             owner             = (int) land[i][j][own_layer] - 1;
             choice            = (int) owner_choice[owner];
             layer             = choice + food_layer_1;
             land[i][j][layer] = get_crop_val(paras);
         }
     }
     
     free(owner_choice);
 }

/* =============================================================================
 * Change the crop choice based on the rotation matrix
 *     C_init:   The matrix for the initial (current) crop positions
 *     C_change: The matrix describing how crop changes occur
 *     paras:    The paras vector that holds global information
 * ========================================================================== */
void change_crop_choice(double **C_init, double **C_change, double *paras){
    
    int i, j, farms, crop_number, now_choice, new_choice;
    
    farms        = (int) paras[142];
    crop_number  = (int) paras[156];
    
    for(i = 0; i < farms; i++){
        now_choice = sample_pr_vector(C_init[i], crop_number);
        new_choice = sample_pr_vector(C_change[now_choice], crop_number);
        for(j = 0; j < crop_number; j++){
            C_init[i][j] = 0.0;
        }
        C_init[i][new_choice] = 1.0;
    }
}

/* =============================================================================
 * Increases the age of each pest by a single time step
 *     land:     The landscape array to be adjusted
 *     paras:    The paras vector that holds global information
 *     ts:       The time step of the simulation
 *     C_init:   The matrix for the initial (current) crop positions
 *     C_change: The matrix describing how crop changes occur
 *     P_init:   The matrix for the initial (current) pesticide positions
 *     P_change: The matrix describing how pesticide changes occur
 * ========================================================================== */
void land_change(double ***land, double *paras, int ts, double **C_init,
                 double **C_change, double **P_init, double **P_change){
  
  int rotate_crops, rotate_pesticide, start_pesticide;
 
  rotate_crops      = (int) paras[143];
  rotate_pesticide  = (int) paras[149];
  start_pesticide   = (int) paras[168];

  if(ts % rotate_crops == 0){
      clean_crops(land, paras);
      change_crop_choice(C_init, C_change, paras);
      init_crop(land, paras, C_init);
  }
  if(ts % rotate_pesticide == 0 && ts >= start_pesticide){
      clean_pesticide(land, paras);
      change_pesticide_choice(P_init, P_change, paras);
      init_pesticide(land, paras, P_init);
  }
}


