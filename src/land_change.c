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
 * Each owner randomly selects a crop to put down
 *     land:        The landscape array to be adjusted
 *     paras:       The paras vector that holds global information
 *     owner_count: Vector of the number
 *     max_own:     The highest ID of a land owner
 * ========================================================================== */
void rand_pesticide(double ***land, double *paras, int *owner_choice, 
                    int max_own){
  
  int i, j, xdim, ydim, owner, own_layer, choice, layer;
  int pesticide_number, pesticide_layer_1;
  
  xdim              = (int) paras[103];
  ydim              = (int) paras[104];
  pesticide_layer_1 = (int) paras[128];
  pesticide_number  = (int) paras[157];
  own_layer         = (int) paras[155];
  
  for(i = 0; i <= max_own; i++){
      owner_choice[i] = get_rand_int(0, pesticide_number - 1);
  }
  
  for(i = 0; i < xdim; i++){
      for(j = 0; j < ydim; j++){
          owner             = (int) land[i][j][own_layer];
          choice            = (int) owner_choice[owner];
          layer             = choice + pesticide_layer_1;  
          land[i][j][layer] = get_pesticide_val(paras);
      }
  }
}

/* =============================================================================
 * Does not rotate the pesticide in any way -- just refereshes it
 *     land:        The landscape array to be adjusted
 *     paras:       The paras vector that holds global information
 *     owner_count: Vector of the number
 *     max_own:     The highest ID of a land owner
 * ========================================================================== */
void no_pest_rot(double ***land, double *paras, int *owner_choice, int max_own){
  
  int i, j, xdim, ydim, pesticide, owner, own_layer, choice, layer;
  int pesticide_number, pesticide_layer_1;
  
  xdim              = (int) paras[103];
  ydim              = (int) paras[104];
  pesticide_layer_1 = (int) paras[128];
  own_layer         = (int) paras[155];
  pesticide_number  = (int) paras[157];
  
  pesticide = 0;
  for(i = 0; i <= max_own; i++){
    owner_choice[i] = pesticide;
    pesticide++;
    if(pesticide >= pesticide_number){
      pesticide = 0;
    }
  }
  
  for(i = 0; i < xdim; i++){
    for(j = 0; j < ydim; j++){
      owner             = (int) land[i][j][own_layer];
      choice            = (int) owner_choice[owner];
      layer             = choice + pesticide_layer_1;
      land[i][j][layer] = get_pesticide_val(paras);
    }
  }
}

/* =============================================================================
 * Changes the pesticide in a specific way
 *     land:    The landscape array to be adjusted
 *     paras:   The paras vector that holds global information
 *     max_own: The maximum ID of an owner
 * ========================================================================== */
void change_pesticide(double ***land, double *paras, int max_own){
  
  int i;
  int pesticide_rotate_type, *owner_choice;
  
  pesticide_rotate_type = (int) paras[148];
  
  owner_choice = malloc(max_own * sizeof(int));
  for(i = 0; i < max_own; i++){
      owner_choice[i] = 0;
  }
  
  switch(pesticide_rotate_type){
      case 0: /* Nothing happens */
          break;
      case 1: /* Heterogeneous pattern but no rotation */
          no_pest_rot(land, paras, owner_choice, max_own);
          break;
      case 2: /* Random selection independent for each owner */
          rand_pesticide(land, paras, owner_choice, max_own);
          break;
      default:
          break;
  }
  
  free(owner_choice);
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
 * Does not rotate the crop in any way -- just refreshes crop on the layer
 *     land:        The landscape array to be adjusted
 *     paras:       The paras vector that holds global information
 *     owner_count: Vector of the number
 *     max_own:     The highest ID of a land owner
 * ========================================================================== */
void no_crop_rot(double ***land, double *paras, int *owner_choice, int max_own){
    
    int i, j, xdim, ydim, crop, owner, own_layer, choice, layer;
    int crop_number, food_layer_1;
    
    xdim         = (int) paras[103];
    ydim         = (int) paras[104];
    food_layer_1 = (int) paras[118];
    own_layer    = (int) paras[155];
    crop_number  = (int) paras[156];
    
    crop = 0;
    for(i = 0; i <= max_own; i++){
        owner_choice[i] = crop;
        crop++;
        if(crop >= crop_number){
          crop = 0;
        }
    }
    
    for(i = 0; i < xdim; i++){
        for(j = 0; j < ydim; j++){
            owner             = (int) land[i][j][own_layer];
            choice            = (int) owner_choice[owner];
            layer             = choice + food_layer_1;
            land[i][j][layer] = get_crop_val(paras);
        }
    }
}

/* =============================================================================
 * Each owner randomly selects a crop to put down
 *     land:        The landscape array to be adjusted
 *     paras:       The paras vector that holds global information
 *     owner_count: Vector of the number
 *     max_own:     The highest ID of a land owner
 * ========================================================================== */
void rand_crop(double ***land, double *paras, int *owner_choice, int max_own){
  
    int i, j, xdim, ydim, owner, own_layer, choice, layer;
    int crop_number, food_layer_1;

    xdim         = (int) paras[103];
    ydim         = (int) paras[104];
    food_layer_1 = (int) paras[118];
    crop_number  = (int) paras[156];
    own_layer    = (int) paras[155];
    
    for(i = 0; i < max_own; i++){
        owner_choice[i] = get_rand_int(0, crop_number - 1);
    }

    for(i = 0; i < xdim; i++){
        for(j = 0; j < ydim; j++){
            owner             = (int) land[i][j][own_layer] - 1;
            choice            = owner_choice[owner];
            layer             = choice + food_layer_1;
            land[i][j][layer] = get_crop_val(paras);
        }
    }
}


/* =============================================================================
 * Changes the crop in a specific way
 *     land:    The landscape array to be adjusted
 *     paras:   The paras vector that holds global information
 *     max_own: The maximum ID of an owner
 * ========================================================================== */
void change_crop(double ***land, double *paras, int max_own){
  
  int i;
  int crop_rotate_type, *owner_choice;
  
  crop_rotate_type = (int) paras[142];
  
  owner_choice = malloc(max_own * sizeof(int));
  for(i = 0; i < max_own; i++){
    owner_choice[i] = 0;
  }
  
  switch(crop_rotate_type){
      case 0: /* Nothing happens */
          break;
      case 1: /* Heterogeneous pattern but no rotation */
          no_crop_rot(land, paras, owner_choice, max_own);
          break;
      case 2: /* Random selection independent for each owner */
          rand_crop(land, paras, owner_choice, max_own);
          break;
      default:
          break;
  }
  
  free(owner_choice);
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
 * Increases the age of each pest by a single time step
 *     land:   The landscape array to be adjusted
 *     paras:  The paras vector that holds global information
 *     ts:     The time step of the simulation
 * ========================================================================== */
void land_change(double ***land, double *paras, int ts){
  
  int i, j, min_own, max_own, own_layer, xdim, ydim, own_val;
  int rotate_crops, rotate_pesticide, start_pesticide;
  
  xdim              = (int) paras[103];
  ydim              = (int) paras[104];
  own_layer         = (int) paras[155];
  rotate_crops      = (int) paras[143];
  rotate_pesticide  = (int) paras[149];
  start_pesticide   = (int) paras[168];
  
  if(ts % rotate_crops == 0 || ts % rotate_pesticide == 0){
      min_own = land[0][0][own_layer];
      max_own = land[0][0][own_layer];
      for(i = 0; i < xdim; i++){
          for(j = 0; j < ydim; j++){
              own_val = (int) land[i][j][own_layer];
              if(own_val < min_own){
                  min_own = own_val;
              }
              if(own_val > max_own){
                  max_own = own_val;
              }
          }
      }
  }

  if(ts % rotate_crops == 0){
      clean_crops(land, paras);
      change_crop(land, paras, max_own);
  }
  if(ts % rotate_pesticide == 0 && ts > start_pesticide){
      clean_pesticide(land, paras);
      change_pesticide(land, paras, max_own);
  }
}



