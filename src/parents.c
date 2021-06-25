#include "utilities.h"

void add_asexual(double **pests, double **offspring, double *paras, int ind,
                 int offspring_count){
    
    int trait, cols, ID;
    int ID_col, age_col, mID_col, mrow_col, off_col, food_col, pest_col;
    int tag1_col, tag2_col, tag3_col, mate_col;
    
    cols     = (int) paras[57];  /* Columns in the pest array             */
    ID_col   = (int) paras[0];   /* Column where the ID is held           */
    age_col  = (int) paras[3];   /* Column where Age is held              */
    mID_col  = (int) paras[6];   /* Column where mum's ID is held         */
    mrow_col = (int) paras[8];   /* Column where mum's row is held        */
    off_col  = (int) paras[10];  /* Column where offspring number is held */
    food_col = (int) paras[14];  /* Column where food intake is held      */
    pest_col = (int) paras[15];  /* Column where pesticide intake is held */
    tag1_col = (int) paras[20];  /* Column where tag 1 is held            */
    tag2_col = (int) paras[21];  /* Column where tag 2 is held            */
    tag3_col = (int) paras[22];  /* Column where tag 3 is held            */
    mate_col = (int) paras[27];  /* Column where mate accessed is held    */
    
    for(trait = 0; trait < cols; trait++){
        offspring[offspring_count][trait] = pests[ind][trait];
    }
    
    offspring[offspring_count][ID_col]    = paras[58] + 1.0; 
    offspring[offspring_count][age_col]   = 0;              
    offspring[offspring_count][mID_col]   = pests[ind][0]; 
    offspring[offspring_count][mrow_col]  = ind;            
    offspring[offspring_count][off_col]   = 0;              
    offspring[offspring_count][food_col]  = 0;            
    offspring[offspring_count][pest_col]  = 0; 
    offspring[offspring_count][tag1_col]  = 0; 
    offspring[offspring_count][tag2_col]  = 0; 
    offspring[offspring_count][tag3_col]  = 0; 
    offspring[offspring_count][mate_col]  = 0;
    
    paras[58]++; /* Increase the maximum ID by 1 */
}


void make_offspring(double **pests, double **offspring, double *paras){
    
    int ind, N, offspring_N, offspring_col, offspring_count;
    int sex_col, sex, selfing_col, selfing;
    int *ind_offspring;
    
    N             = (int) paras[51];
    offspring_N   = (int) paras[56];
    offspring_col = (int) paras[10];
    sex_col       = (int) paras[4];
    selfing_col   = (int) paras[26];
    
    
    ind_offspring = malloc(N * sizeof(int *));
    for(ind = 0; ind < N; ind++){
        ind_offspring[ind] = (int) pests[ind][offspring_col];
    }
    
    offspring_count = 0;
    for(ind = 0; ind < N; ind++){
        sex = (int) pests[ind][sex_col];
        while(ind_offspring[ind] > 0){
            switch(sex){
                case 0: 
                    add_asexual(pests, offspring, paras, ind, offspring_count);
                    break;
                case 1: 
                    selfing = (int) pests[ind][selfing_col];
                    break;
                default:
                    break;
            }
            offspring_count++;
            ind_offspring[ind]--;
        }
    }
    
    free(ind_offspring);
}


