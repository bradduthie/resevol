#include "utilities.h"

/* =============================================================================
 * Pesticide consumed by a single individual on its x-y location
 *     pests: The array holding the parent's information
 *     paras: The paras vector that holds global information
 *     land:  The landscape array with pesticide on it
 *     ind:   The individual of focus that will be eating        
 * ========================================================================== */
void pesticide(double **pests, double *paras, double ***land, int ind){
  
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
  double pesticide_land1, pesticide_land2, pesticide_land3, pesticide_land4;
  double pesticide_land5, pesticide_land6, pesticide_land7, pesticide_land8; 
  double pesticide_land9, pesticide_land10;
  
  xloc_col       = (int) paras[1];
  yloc_col       = (int) paras[2];
  age_col        = (int) paras[3];
  consumed_col   = (int) paras[15];
  min_age_col    = (int) paras[33];
  max_age_col    = (int) paras[34];
  consume_col1   = (int) paras[47];
  consume_col2   = (int) paras[48];
  consume_col3   = (int) paras[49];
  consume_col4   = (int) paras[50];
  consume_col5   = (int) paras[51];
  consume_col6   = (int) paras[52];
  consume_col7   = (int) paras[53];
  consume_col8   = (int) paras[54];
  consume_col9   = (int) paras[55];
  consume_col10  = (int) paras[56];
  land_layer1    = (int) paras[128];
  land_layer2    = (int) paras[129];
  land_layer3    = (int) paras[130];
  land_layer4    = (int) paras[131];
  land_layer5    = (int) paras[132];
  land_layer6    = (int) paras[133];
  land_layer7    = (int) paras[134];
  land_layer8    = (int) paras[135];
  land_layer9    = (int) paras[136];
  land_layer10   = (int) paras[137];
  consumed_col1  = (int) paras[68];
  consumed_col2  = (int) paras[69];
  consumed_col3  = (int) paras[70];
  consumed_col4  = (int) paras[71];
  consumed_col5  = (int) paras[72];
  consumed_col6  = (int) paras[73];
  consumed_col7  = (int) paras[74];
  consumed_col8  = (int) paras[75];
  consumed_col9  = (int) paras[76];
  consumed_col10 = (int) paras[77];
  
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
      if(land_layer1 > 0 && consume1 > 0){
          pesticide_land1 = land[xloc][yloc][land_layer1];
          if(pesticide_land1 < consume1){
              consumed                      += pesticide_land1;
              pests[ind][consumed_col1]     += pesticide_land1;
          }else{
              consumed                      += consume1;
              pests[ind][consumed_col1]     += consume1;
          }
      }
      if(land_layer2 > 0 && consume2 > 0){
          pesticide_land2 = land[xloc][yloc][land_layer1];
          if(pesticide_land2 < consume2){
              consumed                      += pesticide_land2;
              pests[ind][consumed_col2]     += pesticide_land2;
          }else{
              consumed                      += consume2;
              pests[ind][consumed_col2]     += consume2;
          }
      }
      if(land_layer3 > 0 && consume3 > 0){
          pesticide_land1 = land[xloc][yloc][land_layer3];
          if(pesticide_land3 < consume3){
              consumed                      += pesticide_land3;
              pests[ind][consumed_col3]     += pesticide_land3;
          }else{
              consumed                      += consume3;
              pests[ind][consumed_col3]     += consume3;
          }
      }
      if(land_layer4 > 0 && consume4 > 0){
          pesticide_land4 = land[xloc][yloc][land_layer4];
          if(pesticide_land4 < consume4){
              consumed                      += pesticide_land4;
              pests[ind][consumed_col4]     += pesticide_land4;
          }else{
              consumed                      += consume4;
              pests[ind][consumed_col4]     += consume4;
          }
      }
      if(land_layer5 > 0 && consume5 > 0){
          pesticide_land5 = land[xloc][yloc][land_layer5];
          if(pesticide_land5 < consume5){
               consumed                      += pesticide_land5;
               pests[ind][consumed_col5]     += pesticide_land5;
          }else{
               consumed                      += consume5;
               pests[ind][consumed_col5]     += consume5;
          }
      }
      if(land_layer6 > 0 && consume6 > 0){
        pesticide_land6 = land[xloc][yloc][land_layer6];
        if(pesticide_land6 < consume6){
          consumed                      += pesticide_land6;
          pests[ind][consumed_col6]     += pesticide_land6;
        }else{
          consumed                      += consume6;
          pests[ind][consumed_col6]     += consume6;
        }
      }
      if(land_layer7 > 0 && consume7 > 0){
        pesticide_land7 = land[xloc][yloc][land_layer7];
        if(pesticide_land7 < consume7){
          consumed                      += pesticide_land7;
          pests[ind][consumed_col7]     += pesticide_land7;
        }else{
          consumed                      += consume7;
          pests[ind][consumed_col7]     += consume7;
        }
      }
      if(land_layer8 > 0 && consume8 > 0){
        pesticide_land8 = land[xloc][yloc][land_layer8];
        if(pesticide_land8 < consume8){
          consumed                      += pesticide_land8;
          pests[ind][consumed_col8]     += pesticide_land8;
        }else{
          consumed                      += consume8;
          pests[ind][consumed_col8]     += consume8;
        }
      }
      if(land_layer9 > 0 && consume9 > 0){
        pesticide_land9 = land[xloc][yloc][land_layer9];
        if(pesticide_land9 < consume9){
          consumed                      += pesticide_land9;
          pests[ind][consumed_col9]     += pesticide_land9;
        }else{
          consumed                      += consume9;
          pests[ind][consumed_col9]     += consume9;
        }
      }
      if(land_layer10 > 0 && consume10 > 0){
        pesticide_land10 = land[xloc][yloc][land_layer10];
        if(pesticide_land10 < consume10){
          consumed                       += pesticide_land10;
          pests[ind][consumed_col10]     += pesticide_land10;
        }else{
          consumed                       += consume10;
          pests[ind][consumed_col10]     += consume10;
        }
      }
  }
  pests[ind][consumed_col] += consumed;
}

/* =============================================================================
 * Pesticide consumed for all individuals on their x-y locations
 *     pests: The array holding the parent's information
 *     paras: The paras vector that holds global information
 *     land:  The landscape array with pesticide on it
 * ========================================================================== */
void pesticide_consumed(double **pests, double *paras, double ***land){
  
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
      pesticide(pests, paras, land, ind);
      not_fed[ind] = 0;
      N_count--;
  }
  
  free(not_fed);
}






