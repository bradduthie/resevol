#include "utilities.h"

/* =============================================================================
 * Gets an actual value for the crop being inserted into the landscape
 *     paras:  The paras vector that holds global information
 * ========================================================================== */
int get_crop_val(double *paras){

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
 * Does not rotate the crop in any way -- just refereshes crop on the layer
 *     land:        The landscape array to be adjusted
 *     paras:       The paras vector that holds global information
 *     owner_count: Vector of the number
 *     max_own:     The highest ID of a land owner
 * ========================================================================== */
void no_rotation(double ***land, double *paras, int *owner_choice, int max_own){
    
    int i, j, xdim, ydim, crop, owner, own_layer, choice, layer;
    int crop_number, food_layer_1;
    
    xdim         = (int) paras[103];
    ydim         = (int) paras[104];
    food_layer_1 = (int) paras[118];
    crop_number  = (int) paras[156];
    own_layer    = (int) paras[155];
    
    crop = 0;
    for(i = 0; i < max_own; i++){
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
 * Changes the crop in a specific way for a given layer
 *     land:    The landscape array to be adjusted
 *     paras:   The paras vector that holds global information
 *     max_own: The maximum ID of an owner
 * ========================================================================== */
void change_crop(double ***land, double *paras, int max_own){
  
  int i, j;
  int crop_rotate_type, *owner_choice;
  
  crop_rotate_type = (int) paras[142];
  
  owner_choice = malloc(max_own * sizeof(int));
  for(i = 0; i < max_own; i++){
    owner_choice[i] = 0;
  }
  
  switch(crop_rotate_type){
      case 0:
          break;
      case 1:
          no_rotation(land, paras, owner_choice, max_own);
          break;
      default:
          break;
  }
  
  free(owner_choice);
}


/* =============================================================================
 * Increases the age of each pest by a single time step
 *     land:   The landscape array to be adjusted
 *     paras:  The paras vector that holds global information
 * ========================================================================== */
void land_change(double ***land, double *paras){
  
  int i, j, k, layer, min_own, max_own, own_layer, xdim, ydim, own_val;
  int crops_produced, pesticides_used, possible_crops, possible_pesti;
  int *owner_choice, owner;
  
  xdim            = (int) paras[103];
  ydim            = (int) paras[104];
  own_layer       = (int) paras[155];
  crops_produced  = (int) paras[156];
  pesticides_used = (int) paras[157];
  possible_crops  = (int) paras[158];
  possible_pesti  = (int) paras[159];
  
  min_own = land[0][0][own_layer];
  for(i = 0; i < xdim; i++){
      for(j = 0; j < ydim; j++){
          own_val = (int) land[j][i][own_layer];
          if(own_val < min_own){
              min_own = own_val;
          }
          if(own_val > max_own){
              max_own = own_val;
          }
      }
  }
  
  change_crop(land, paras, max_own);
 
}



