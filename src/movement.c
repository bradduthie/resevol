#include "utilities.h"
#include "feeding.h"
#include "pesticide.h"

/* =============================================================================
 * This function applies the edge effect during movement
 * pos:       Current position on one dimension of the landscape
 * edge_1:    The location of the lowest edge of the landscape
 * edge_2:    The location of the highest edge of the landscape
 * edge_type: The type of edge being modelled (e.g., torus)
 * ========================================================================== */
int edge_effect(int pos, int edge_1, int edge_2, int edge_type){
    if(pos >= edge_2 || pos < edge_1){ /* If off the edge */
        switch(edge_type){
            case 0: /* Torus landscape */
                while(pos > edge_2){
                    pos = pos - edge_2;   
                }
                while(pos < edge_1){
                    pos = pos + edge_2;   
                }
                break;
            default:
                while(pos > edge_2){
                    pos = pos - edge_2;   
                }
                while(pos < edge_1){
                    pos = pos + edge_2;   
                }
                break;
        }
    }
    return pos;
}

/* =============================================================================
 * Moves a single individual on the x-y location where it is located
 *     pests: The array holding the parent's information
 *     paras: The paras vector that holds global information
 *     ind:   The individual of focus that will be moving        
 * ========================================================================== */
void move(double **pests, double *paras, int ind){
  
  int N, age_col, move_dist_col, min_age_col, max_age_col, Xpos_col, Ypos_col;
  int move_dist, min_age, max_age, land_type, X_dim, Y_dim, disp_x, disp_y;
  int Xpos, Ypos, new_xpos, new_ypos, age;
  
  N               = (int) paras[101];
  Xpos_col        = (int) paras[1];
  Ypos_col        = (int) paras[2];
  age_col         = (int) paras[3];
  move_dist_col   = (int) paras[5];
  min_age_col     = (int) paras[31];
  max_age_col     = (int) paras[32];
  
  land_type = (int) paras[102];
  X_dim     = (int) paras[103];
  Y_dim     = (int) paras[104];
  age       = (int) pests[ind][age_col];
  Xpos      = (int) pests[ind][Xpos_col];
  Ypos      = (int) pests[ind][Ypos_col];
  move_dist = (int) pests[ind][move_dist_col];
  min_age   = (int) pests[ind][min_age_col];
  max_age   = (int) pests[ind][max_age_col];
  
  if(age >= min_age && age <= max_age){
      disp_x   = get_rand_int(Xpos - move_dist, Xpos + move_dist);
      new_xpos = edge_effect(disp_x, 0, X_dim, land_type);
      disp_y   = get_rand_int(Ypos - move_dist, Ypos + move_dist);
      new_ypos = edge_effect(disp_y, 0, Y_dim, land_type);
      pests[ind][Xpos_col] = new_xpos;
      pests[ind][Ypos_col] = new_ypos;
  }
}

/* =============================================================================
 * Moves a single individual on the x-y location where it is located
 *     pests: The array holding the parent's information
 *     paras: The paras vector that holds global information
 *     land:  The landscape array with food on it
 * ========================================================================== */
void movement(double **pests, double *paras, double ***land){
  
  int ind, N, age_col, min_age_col, max_age_col, bout_col, eat_on_bout_col;
  int age, bout, min_age, max_age, max_bout, ind_bout, tot_bouts, *bout_vec;
  int eat_on_bout, pesticide_on_bout, cide_on_bout_col;
  
  N                = (int) paras[101];
  age_col          = (int) paras[3];
  bout_col         = (int) paras[30];
  min_age_col      = (int) paras[31];
  max_age_col      = (int) paras[32];
  eat_on_bout_col  = (int) paras[57];
  cide_on_bout_col = (int) paras[78];
  
  max_bout = 0;
  for(ind = 0; ind < N; ind++){
      ind_bout = (int) pests[ind][bout_col];
      age      = (int) pests[ind][age_col];
      min_age  = (int) pests[ind][min_age_col];
      max_age  = (int) pests[ind][max_age_col];
      if(ind_bout > max_bout && age >= min_age && age <= max_age){
        max_bout = ind_bout;
      }
  }
  
  if(max_bout == 1){
      for(ind = 0; ind < N; ind++){
        move(pests, paras, ind);
      }
  }
  
  if(max_bout > 1){
      eat_on_bout       = pests[ind][eat_on_bout_col];
      pesticide_on_bout = pests[ind][cide_on_bout_col]; 
      bout_vec          = malloc(N * sizeof(int));
      tot_bouts         = 0;
      for(ind = 0; ind < N; ind++){
        bout_vec[ind]  = (int) pests[ind][bout_col];
        tot_bouts     += bout_vec[ind];
      }
      while(tot_bouts > 0){
        do{ /* This makes movement happen in a random order */
          ind = get_rand_int(0, N);
        } while (bout_vec[ind] == 0);
        move(pests, paras, ind);
        if(eat_on_bout > 0){
            feed(pests, paras, land, ind);
        }
        if(pesticide_on_bout > 0){
            pesticide(pests, paras, land, ind);
        }
        bout_vec[ind]--;
        tot_bouts--;
      }
      free(bout_vec);
  }
}





