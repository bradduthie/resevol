#include "utilities.h"

int mate_in_range(double **pests, double *paras, int row, int mate_sex){
  
  int ind, N, range_col, range, xcol, ycol, sex_col, land_type, xdim, ydim;
  int focal_x, focal_y, other_x, other_y, other_sex;
  int xdist, ydist, xdist_2, ydist_2, xdist_3, ydist_3, in_range;
  
  xcol      = (int) paras[1];
  ycol      = (int) paras[2];
  sex_col   = (int) paras[4];
  N         = (int) paras[31];
  range_col = (int) paras[24];
  range     = pests[row][range_col];
  land_type = (int) paras[32];
  xdim      = (int) paras[33];
  ydim      = (int) paras[34];
  
  focal_x   = pests[row][xcol];
  focal_y   = pests[row][ycol];
  
  in_range  = 0;
  for(ind = 0; ind < N; ind++){
    other_x   = pests[ind][xcol];
    other_y   = pests[ind][ycol];
    other_sex = pests[ind][sex_col];
    xdist     = abs(focal_x - other_x);
    ydist     = abs(focal_y - other_y);
    if(land_type == 0){ /* What follows finds the distance given the torus */
      xdist_2 = abs((focal_x + xdim) - other_x);
      ydist_2 = abs((focal_y + ydim) - other_y);
      xdist_3 = abs(focal_x - (other_x + xdim));
      ydist_3 = abs(focal_y - (other_y + ydim));
      if(xdist_2 < xdist){
        xdist = xdist_2;
      }
      if(xdist_3 < xdist){
        xdist = xdist_3;
      }
      if(ydist_2 < ydist){
        ydist = ydist_2;
      }
      if(ydist_3 < ydist){
        ydist = ydist_3;
      }
    }
    if(other_sex == mate_sex && xdist <= range && ydist <= range){
      in_range = 1;
      break;
    }
  }
  
  return in_range;
}

int mate_available(double **pests, double *paras, int row){
  
  int N, ind, sex, sex_col, mate_found,selfing_col, selfing;
  
  N            = (int) paras[31];
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
          if(selfing == 1){
              mate_found = 1;
          }else{
              mate_found = mate_in_range(pests, paras, row, 1);
              if(mate_found == 0){ /* Can also look for just a male */
                  mate_found = mate_in_range(pests, paras, row, 3);
              }
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

void count_offspring(double **pests, double *paras, int row){
  
  int N, repr_param_col, repr_param, offspring, repr_type_col, repr_type;
  int mate_access, mate_access_col, sex_col, sex, off_col;
  
  N               = (int) paras[31];
  sex_col         = (int) paras[4];
  sex             = (int) pests[row][sex_col];
  repr_type_col   = (int) paras[23];
  repr_type       = (int) pests[row][repr_type_col];
  repr_param_col  = (int) paras[25];
  repr_param      = pests[row][repr_param_col];
  mate_access_col = (int) paras[27];
  mate_access     = 0;
  off_col         = (int) paras[10];
  
  switch(repr_type){
      case 0:  /* Reproduction is just based off of lambda value */
          mate_access = mate_available(pests, paras, row);
          if(mate_access > 0){
              offspring = rpois(repr_param);
          }
          break;
      case 1: /* Offspring will be based off of food consumed */
          offspring        = 1;
          break;
      default:
          mate_access = mate_available(pests, paras, row);
          if(mate_access > 0){
              offspring = rpois(repr_param);
          }
          break;
  }
  
  pests[row][mate_access_col] = mate_access;
  if(sex < 3){
    pests[row][off_col] = offspring;
  }
  paras[36] += (double) offspring;
}

void calculate_offspring(double **pests, double *paras){
  
  int ind, N;

  N                 = (int) paras[31];
  
  paras[36] = 0.0; /* Start with no offspring */
  
  for(ind = 0; ind < N; ind++){
    count_offspring(pests, paras, ind);
  }
}



