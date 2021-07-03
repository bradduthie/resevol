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
  int land_layer1, land_layer2, land_layer3, land_layer4, land_layer5;
  int xloc, yloc, min_age, max_age, age_col, age;
  double consume1, consume2, consume3, consume4, consume5, consumed;
  double food_land1, food_land2, food_land3, food_land4, food_land5;
  
  xloc_col      = (int) paras[1];
  yloc_col      = (int) paras[2];
  age_col       = (int) paras[3];
  consumed_col  = (int) paras[14];
  min_age_col   = (int) paras[33];
  max_age_col   = (int) paras[34];
  consume_col1  = (int) paras[37];
  consume_col2  = (int) paras[38];
  consume_col3  = (int) paras[39];
  consume_col4  = (int) paras[40];
  consume_col5  = (int) paras[41];
  land_layer1   = (int) paras[68];
  land_layer2   = (int) paras[69];
  land_layer3   = (int) paras[70];
  land_layer4   = (int) paras[71];
  land_layer5   = (int) paras[72];
  
  xloc     = (int) pests[ind][xloc_col];
  yloc     = (int) pests[ind][yloc_col];
  age      = (int) pests[ind][age_col];
  min_age  = (int) pests[ind][min_age_col];
  max_age  = (int) pests[ind][max_age_col];
  consume1 = pests[ind][consume_col1];
  consume2 = pests[ind][consume_col2];
  consume3 = pests[ind][consume_col3];
  consume4 = pests[ind][consume_col4];
  consume5 = pests[ind][consume_col5];
  
  consumed = 0.0;
  if(age >= min_age && age <= max_age){
      if(land_layer1 > 0){
          food_land1 = land[xloc][yloc][land_layer1];
          if(food_land1 < consume1){
              consumed                      = food_land1;
              land[xloc][yloc][land_layer1] = 0;
          }else{
              consumed                      += consume1;
              land[xloc][yloc][land_layer1] -= consume1;
          }
      }
      if(land_layer2 > 0){
          food_land2 = land[xloc][yloc][land_layer1];
          if(food_land2 < consume1){
              consumed                      = food_land2;
              land[xloc][yloc][land_layer2] = 0;
          }else{
              consumed                      += consume2;
              land[xloc][yloc][land_layer2] -= consume2;
          }
      }
      if(land_layer3 > 0){
          food_land1 = land[xloc][yloc][land_layer3];
          if(food_land3 < consume3){
              consumed                      = food_land3;
              land[xloc][yloc][land_layer3] = 0;
          }else{
              consumed                      += consume3;
              land[xloc][yloc][land_layer3] -= consume3;
          }
      }
      if(land_layer4 > 0){
          food_land4 = land[xloc][yloc][land_layer4];
          if(food_land4 < consume4){
              consumed                      = food_land4;
              land[xloc][yloc][land_layer4] = 0;
          }else{
              consumed                      += consume4;
              land[xloc][yloc][land_layer4] -= consume4;
          }
      }
      if(land_layer5 > 0){
          food_land5 = land[xloc][yloc][land_layer1];
          if(food_land5 < consume1){
               consumed                      = food_land5;
               land[xloc][yloc][land_layer5] = 0;
          }else{
               consumed                      += consume5;
               land[xloc][yloc][land_layer5] -= consume5;
          }
      }
  }
}

/* =============================================================================
 * Feeds all individuals on the x-y location where it is located
 *     pests: The array holding the parent's information
 *     paras: The paras vector that holds global information
 *     land:  The landscape array with food on it
 * ========================================================================== */
void feeding(double **pests, double *paras, double ***land){
  
  int ind, N;
  
  N = (int) paras[51];
  
  for(ind = 0; ind < N; ind++){
    feed(pests, paras, land, ind);
  }
}

