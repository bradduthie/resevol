#include "utilities.h"

/* =============================================================================
 * Assigns the offspring to a sire based
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 *     ind:             The row of the mother of the individual
 * ========================================================================== */
int assign_sire(double **pests, double *paras, int ind){
    
    int i, N, in_range, opp_sex, range;
    int mate_col, sex_col, range_col, self_col;
    int nearby_mates, mate_pos, mate_row, mate_sex, focal_sex, selfing;
    
    sex_col   = (int) paras[4];  /* Column where the sex of individual is     */
    mate_col  = (int) paras[27]; /* Column for number of mates accessible     */
    range_col = (int) paras[24];
    self_col  = (int) paras[26];
    
    N            = (int) paras[51];
    nearby_mates = (int) pests[ind][mate_col];
    focal_sex    = (int) pests[ind][sex_col];
    
    range        = pests[ind][range_col];
    selfing      = pests[ind][self_col];
    
    mate_sex     = 1;
    if(focal_sex == 2){
        mate_sex = 3;
    }
    /* Quickly get a random avail sire: replace with function for mate choice */
    mate_pos = 1;
    if(nearby_mates > 1){
        mate_pos = get_rand_int(1, nearby_mates);
    }
    while(N > 0 && mate_pos > 0){
        N--;
        in_range = is_in_range(pests, ind, N, paras, range);
        opp_sex  = pests[N][sex_col];
        if(in_range > 0 && opp_sex == mate_sex){
            if(N != ind || selfing > 0){
                mate_pos--;
            }
        }
        
    }
    printf("%f\t%f\t%f\t%f\t%f\t%f\t%d\t%d\n", pests[ind][0], pests[N][0], pests[ind][1], pests[ind][2], pests[N][1], pests[N][2], N, ind);
    
    return 0; /* The while loop above finds the row of the mate */
}

/* =============================================================================
 * Adds sexual individuals into the population given parent info
 *     pests:           The array holding the parent's information
 *     offspring:       The array that will hold the offspring's information
 *     paras:           The paras vector that holds global information
 *     ind:             The row of the mother of the individual
 *     offspring_count: The number of individuals in the offspring array (rows)
 * ========================================================================== */
void add_sexual(double **pests, double **offspring, double *paras, int ind,
                int offspring_count){
    
    int trait, cols, ID, sire_row, sire_ID, srow_col, sID_col;
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
    srow_col = (int) paras[9];   /* Column where the sire's row is held   */ 
    sID_col  = (int) paras[7];   /* Column where the sire's ID is held    */
    
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
    
    sire_row = assign_sire(pests, paras, ind);
    sire_ID  = pests[sire_row][ID_col];
    
    offspring[offspring_count][srow_col] = sire_row;
    offspring[offspring_count][sID_col]  = sire_ID;
    
    paras[58]++; /* Increase the maximum ID by 1 */
}


/* =============================================================================
 * Adds asexual individuals into the population as clones of their parent
 *     pests:           The array holding the parent's information
 *     offspring:       The array that will hold the offspring's information
 *     paras:           The paras vector that holds global information
 *     ind:             The row of the mother of the individual
 *     offspring_count: The number of individuals in the offspring array (rows)
 * ========================================================================== */
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


/* =============================================================================
 * Places the offspring in the offspring array depending on reproduction type
 *     pests:     The array holding the parent's information
 *     offspring: The array that will hold the offspring's information
 *     paras:     The paras vector that holds global information
 * ========================================================================== */
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
                    add_sexual(pests, offspring, paras, ind, offspring_count);
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


