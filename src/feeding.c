#include "utilities.h"

/* =============================================================================
 * Feeds a single individual on the x-y location where it is located
 *     pests: The array holding the parent's information
 *     paras: The paras vector that holds global information
 *     land:  The landscape array with food on it
 *     ind:   The individual of focus that will be eating        
 * ========================================================================== */
void feed(double **pests, double *paras, double ***land, int ind){
  
  int min_age_col, max_age_col, xloc_col, yloc_col, consumed_col;
  int consume_col1, consume_col2, consume_col3, consume_col4, consume_col5;
  int consume_col6, consume_col7, consume_col8, consume_col9, consume_col10;
  int consumed_col1, consumed_col2, consumed_col3, consumed_col4, consumed_col5;
  int consumed_col6, consumed_col7, consumed_col8, consumed_col9;
  int land_layer1, land_layer2, land_layer3, land_layer4, land_layer5;
  int land_layer6, land_layer7, land_layer8, land_layer9, land_layer10;
  int xloc, yloc, min_age, max_age, age_col, age, consumed_col10;;
  double consume1, consume2, consume3, consume4, consume5, consumed;
  double consume6, consume7, consume8, consume9, consume10;
  double food_land1, food_land2, food_land3, food_land4, food_land5;
  double food_land6, food_land7, food_land8, food_land9, food_land10;
  
  xloc_col       = (int) paras[1];
  yloc_col       = (int) paras[2];
  age_col        = (int) paras[3];
  consumed_col   = (int) paras[14];
  min_age_col    = (int) paras[33];
  max_age_col    = (int) paras[34];
  consume_col1   = (int) paras[37];
  consume_col2   = (int) paras[38];
  consume_col3   = (int) paras[39];
  consume_col4   = (int) paras[40];
  consume_col5   = (int) paras[41];
  consume_col6   = (int) paras[42];
  consume_col7   = (int) paras[43];
  consume_col8   = (int) paras[44];
  consume_col9   = (int) paras[45];
  consume_col10  = (int) paras[46];
  land_layer1    = (int) paras[118];
  land_layer2    = (int) paras[119];
  land_layer3    = (int) paras[120];
  land_layer4    = (int) paras[121];
  land_layer5    = (int) paras[122];
  land_layer6    = (int) paras[123];
  land_layer7    = (int) paras[124];
  land_layer8    = (int) paras[125];
  land_layer9    = (int) paras[126];
  land_layer10   = (int) paras[127];
  consumed_col1  = (int) paras[58];
  consumed_col2  = (int) paras[59];
  consumed_col3  = (int) paras[60];
  consumed_col4  = (int) paras[61];
  consumed_col5  = (int) paras[62];
  consumed_col6  = (int) paras[63];
  consumed_col7  = (int) paras[64];
  consumed_col8  = (int) paras[65];
  consumed_col9  = (int) paras[66];
  consumed_col10 = (int) paras[67];
  
  xloc      = (int) pests[ind][xloc_col];
  yloc      = (int) pests[ind][yloc_col];
  age       = (int) pests[ind][age_col];
  min_age   = (int) pests[ind][min_age_col];
  max_age   = (int) pests[ind][max_age_col];
  consume1  = pests[ind][consume_col1];
  consume2  = pests[ind][consume_col2];
  consume3  = pests[ind][consume_col3];
  consume4  = pests[ind][consume_col4];
  consume5  = pests[ind][consume_col5];
  consume6  = pests[ind][consume_col6];
  consume7  = pests[ind][consume_col7];
  consume8  = pests[ind][consume_col8];
  consume9  = pests[ind][consume_col9];
  consume10 = pests[ind][consume_col10];
  
  consumed = 0.0;
  if(age >= min_age && age <= max_age){
      if(land[xloc][yloc][land_layer1] > 0 && consume1 > 0){
          food_land1 = land[xloc][yloc][land_layer1];
          if(food_land1 < consume1){
              consumed                      += food_land1;
              land[xloc][yloc][land_layer1]  = 0;
              pests[ind][consumed_col1]     += food_land1;
          }else{
              consumed                      += consume1;
              land[xloc][yloc][land_layer1] -= consume1;
              pests[ind][consumed_col1]     += consume1;
          }
      }
      if(land[xloc][yloc][land_layer2] > 0 && consume2 > 0){
          food_land2 = land[xloc][yloc][land_layer2];
          if(food_land2 < consume2){
              consumed                      += food_land2;
              land[xloc][yloc][land_layer2]  = 0;
              pests[ind][consumed_col2]     += food_land2;
          }else{
              consumed                      += consume2;
              land[xloc][yloc][land_layer2] -= consume2;
              pests[ind][consumed_col2]     += consume2;
          }
      }
      if(land[xloc][yloc][land_layer3] > 0 && consume3 > 0){
          food_land3 = land[xloc][yloc][land_layer3];
          if(food_land3 < consume3){
              consumed                      += food_land3;
              land[xloc][yloc][land_layer3]  = 0;
              pests[ind][consumed_col3]     += food_land3;
          }else{
              consumed                      += consume3;
              land[xloc][yloc][land_layer3] -= consume3;
              pests[ind][consumed_col3]     += consume3;
          }
      }
      if(land[xloc][yloc][land_layer4] > 0 && consume4 > 0){
          food_land4 = land[xloc][yloc][land_layer4];
          if(food_land4 < consume4){
              consumed                      += food_land4;
              land[xloc][yloc][land_layer4]  = 0;
              pests[ind][consumed_col4]     += food_land4;
          }else{
              consumed                      += consume4;
              land[xloc][yloc][land_layer4] -= consume4;
              pests[ind][consumed_col4]     += consume4;
          }
      }
      if(land[xloc][yloc][land_layer5] > 0 && consume5 > 0){
          food_land5 = land[xloc][yloc][land_layer5];
          if(food_land5 < consume5){
               consumed                      += food_land5;
               land[xloc][yloc][land_layer5]  = 0;
               pests[ind][consumed_col5]     += food_land5;
          }else{
               consumed                      += consume5;
               land[xloc][yloc][land_layer5] -= consume5;
               pests[ind][consumed_col5]     += consume5;
          }
      }
      if(land[xloc][yloc][land_layer6] > 0 && consume6 > 0){
          food_land6 = land[xloc][yloc][land_layer6];
          if(food_land6 < consume6){
              consumed                      += food_land6;
              land[xloc][yloc][land_layer6]  = 0;
              pests[ind][consumed_col6]     += food_land6;
          }else{
              consumed                      += consume6;
              land[xloc][yloc][land_layer6] -= consume6;
              pests[ind][consumed_col6]     += consume6;
          }
      }
      if(land[xloc][yloc][land_layer7] > 0 && consume7 > 0){
          food_land7 = land[xloc][yloc][land_layer7];
          if(food_land7 < consume7){
              consumed                      += food_land7;
              land[xloc][yloc][land_layer7]  = 0;
              pests[ind][consumed_col7]     += food_land7;
          }else{
              consumed                      += consume7;
              land[xloc][yloc][land_layer7] -= consume7;
              pests[ind][consumed_col7]     += consume7;
        }
      }
      if(land[xloc][yloc][land_layer8] > 0 && consume8 > 0){
          food_land8 = land[xloc][yloc][land_layer8];
          if(food_land8 < consume8){
              consumed                      += food_land8;
              land[xloc][yloc][land_layer8]  = 0;
              pests[ind][consumed_col8]     += food_land8;
          }else{
              consumed                      += consume8;
              land[xloc][yloc][land_layer8] -= consume8;
              pests[ind][consumed_col8]     += consume8;
          }
      }
      if(land[xloc][yloc][land_layer9] > 0 && consume9 > 0){
          food_land9 = land[xloc][yloc][land_layer9];
          if(food_land9 < consume9){
              consumed                      += food_land9;
              land[xloc][yloc][land_layer9]  = 0;
              pests[ind][consumed_col9]     += food_land9;
          }else{
              consumed                      += consume9;
              land[xloc][yloc][land_layer9] -= consume9;
              pests[ind][consumed_col9]     += consume9;
          }
      }
      if(land[xloc][yloc][land_layer10] > 0 && consume10 > 0){
          food_land10 = land[xloc][yloc][land_layer10];
          if(food_land10 < consume10){
              consumed                       += food_land10;
              land[xloc][yloc][land_layer10]  = 0;
              pests[ind][consumed_col10]     += food_land10;
          }else{
              consumed                       += consume10;
              land[xloc][yloc][land_layer10] -= consume10;
              pests[ind][consumed_col10]     += consume10;
          }
      }
      pests[ind][consumed_col] += consumed;
  }
}

/* =============================================================================
 * Feeds all individuals on the x-y location where it is located
 *     pests: The array holding the parent's information
 *     paras: The paras vector that holds global information
 *     land:  The landscape array with food on it
 * ========================================================================== */
void feeding(double **pests, double *paras, double ***land){
  
  int ind, N, *not_fed, N_count;
  
  N       = (int) paras[101];
  not_fed = malloc(N * sizeof(int));
  for(ind = 0; ind < N; ind++){
      not_fed[ind] = 1;
  }
  
  N_count = N;
  while(N_count > 0){
      do{
           ind = get_rand_int(0, N - 1);
      } while (not_fed[ind] == 0);
      feed(pests, paras, land, ind);
      not_fed[ind] = 0;
      N_count--;
  }
  
  free(not_fed);
}

