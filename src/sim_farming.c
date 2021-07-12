#include <time.h>
#include "utilities.h"
#include "reproduction.h"
#include "parents.h"
#include "age.h"
#include "feeding.h"
#include "movement.h"
#include "pesticide.h"
#include "mortality.h"
#include "fill_new_pests.h"
#include "land_change.h"
#include "statistics.h"

/* =============================================================================
 * This is the outer function for simulating farming and pesticide resistance
 *  Inputs include:
 *      IND:     Array with individual pest data
 *      LAND:    Three dimensional array of landscape
 *      PARAS:   Nothing yet, but will hold the paramters of interest
 * ===========================================================================*/
SEXP sim_farming(SEXP IND, SEXP LAND, SEXP PARAS){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int    i, j, k, ts;
    int    row, col;
    int    xloc, yloc, zloc;
    int    land_z, land_y, land_x;
    int    vec_pos;
    int    time_steps;
    int    ind_number;
    int    ind_traits;
    int    offspring_number; /* New number of individuals post reproduction */
    int    new_total_N;      /* Total number of individuals after a time step */
    int    surviving_N;      /* Surviving individuals after a time step */
    int    protected_n;      /* Number of protected R objects */
    int    len_PARAS;        /* Length of the parameters vector */
    int    print_gen;        /* Should the generations be printed */
    int    *dim_IND;         /* Dimensions of the individual array */
    int    *dim_LAND;        /* Dimensions of the landscape */
  
    double *paras_ptr;
    double *IND_ptr;
    double *LAND_ptr;
    double *paras;
    double **pests;        /* The pests array */
    double **offspring;    /* The offspring of pests within a time step */
    double **new_pests;    /* The pest array at the end of a time step */
    double ***land;        /* The landscape array */
    double *paras_ptr_new; /* Pointer to new paras (interface R and C) */
    double *land_ptr_new;  /* Pointer to LAND_NEW (interface R and C) */
    double time_spent;
    
    clock_t begin;
    clock_t end;

    begin = clock();
    
    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */

    protected_n = 0;
    
    PROTECT( PARAS = AS_NUMERIC(PARAS) );
    protected_n++;
    paras_ptr = REAL(PARAS);
    
    PROTECT( IND = AS_NUMERIC(IND) );
    protected_n++;
    IND_ptr = REAL(IND);
    
    PROTECT( LAND = AS_NUMERIC(LAND) );
    protected_n++;
    LAND_ptr = REAL(LAND);
    
    len_PARAS   = GET_LENGTH(PARAS);
    dim_IND     = INTEGER( GET_DIM(IND) );
    dim_LAND    = INTEGER( GET_DIM(LAND) );
  
    
    /* The C code for the model itself falls under here */
    /* ====================================================================== */
    
    paras   = malloc(len_PARAS * sizeof(double *));
    vec_pos = 0;
    for(i = 0; i < len_PARAS; i++){
        paras[i] = paras_ptr[vec_pos];
        vec_pos++; 
    } /* The parameters vector is now copied into C */

    /* Code below remakes the IND matrix for easier use */
    ind_number = dim_IND[0];
    ind_traits = dim_IND[1];
    
    pests  = malloc(ind_number * sizeof(double *));
    for(row = 0; row < ind_number; row++){
        pests[row] = malloc(ind_traits * sizeof(double));   
    } 
    vec_pos = 0;
    for(col = 0; col < ind_traits; col++){
        for(row = 0; row < ind_number; row++){
            pests[row][col] = IND_ptr[vec_pos]; 
            vec_pos++;
        }
    }
     
    /* Code below remakes the LAND array for easier use */
    land_x = dim_LAND[0];
    land_y = dim_LAND[1];
    land_z = dim_LAND[2];   
    
    land   = malloc(land_x * sizeof(double *));
    for(xloc = 0; xloc < land_x; xloc++){
      land[xloc] = malloc(land_y * sizeof(double *));
      for(yloc = 0; yloc < land_y; yloc++){
        land[xloc][yloc] = malloc(land_z * sizeof(double));   
      }
    } 
    vec_pos = 0;
    for(zloc = 0; zloc < land_z; zloc++){
      for(yloc = 0; yloc < land_y; yloc++){
        for(xloc = 0; xloc < land_x; xloc++){
          land[xloc][yloc][zloc] = LAND_ptr[vec_pos];
          vec_pos++;
        }
      }
    }  /* LAND is now stored as land */   
    
    /* Do the biology here now */
    /* ====================================================================== */

    print_gen  = (int) paras[165];
    time_steps = (int) paras[140];
    ts         = 0;
    
    while(ts < time_steps){
        
        land_change(land, paras, ts);
 
        age_pests(pests, paras); 
        
        feeding(pests, paras, land); 

        pesticide_consumed(pests, paras, land); 
    
        movement(pests, paras, land); 
    
        calculate_offspring(pests, paras);
    
        offspring_number = (int) paras[106]; /* Create the offspring array */
        offspring        = malloc(offspring_number * sizeof(double *));
        for(row = 0; row < offspring_number; row++){
            offspring[row] = malloc(ind_traits * sizeof(double));   
        } 

        make_offspring(pests, offspring, paras);
    
        apply_mortality(pests, paras);
    
        print_all_pests(pests, paras, ts);
        population_statistics(pests, paras, ts);
    
        surviving_N = (int) paras[138];
        new_total_N = offspring_number + surviving_N;
        paras[139]  = (double) new_total_N; 

        if(new_total_N < 6){
            for(row = 0; row < offspring_number; row++){
                free(offspring[row]);
            }
            free(offspring);
            paras[141] = 1;
            break;
        }
        
        new_pests   = malloc(new_total_N * sizeof(double *));
        for(row = 0; row < new_total_N; row++){
            new_pests[row] = malloc(ind_traits * sizeof(double));   
        } 
        
        fill_new_pests(pests, offspring, new_pests, paras);
        
        for(row = 0; row < offspring_number; row++){
            free(offspring[row]);
        }
        free(offspring);
        
        ind_number = (int) paras[101];
        for(row = 0; row < ind_number; row++){
            free(pests[row]);
        }
        free(pests);
        
        paras[101] = (double) new_total_N; 
        pests      = malloc(new_total_N * sizeof(double *));
        for(row = 0; row < new_total_N; row++){
            pests[row] = malloc(ind_traits * sizeof(double));   
        } 
        
        for(row = 0; row < new_total_N; row++){
            for(col = 0; col < ind_traits; col++){
                pests[row][col] = new_pests[row][col];
            }
        }
        
        for(row = 0; row < new_total_N; row++){
            free(new_pests[row]);
        }
        free(new_pests);

        ts++;
        
        if(print_gen > 0){
            Rprintf("%d\t%d\n", ts, new_total_N);
        }
    }
    
    end        = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    paras[163] = time_spent;
    
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP PARAMETERS_NEW;
    PROTECT( PARAMETERS_NEW = allocVector(REALSXP, len_PARAS) );
    protected_n++;
    
    paras_ptr_new = REAL(PARAMETERS_NEW);
    
    vec_pos = 0;
    for(i = 0; i < len_PARAS; i++){
        paras_ptr_new[vec_pos] = paras[i];
        vec_pos++;
    }
  
    SEXP LAND_NEW;
    PROTECT( LAND_NEW = alloc3DArray(REALSXP, land_x, land_y, land_z) );
    protected_n++;
    
    land_ptr_new = REAL(LAND_NEW);
    
    vec_pos = 0;
    for(zloc = 0; zloc < land_z; zloc++){
      for(yloc = 0; yloc < land_y; yloc++){
        for(xloc = 0; xloc < land_x; xloc++){
          land_ptr_new[vec_pos] = land[xloc][yloc][zloc];
          vec_pos++;
        }
      }
    }
  
    SEXP OUTPUT;
    OUTPUT = PROTECT( allocVector(VECSXP, 2) );
    protected_n++;
    SET_VECTOR_ELT(OUTPUT, 0, PARAMETERS_NEW);
    SET_VECTOR_ELT(OUTPUT, 1, LAND_NEW);

    UNPROTECT(protected_n);
  
    
     
    /* Free all of the allocated memory used in arrays */
    for(xloc = 0; xloc < land_x; xloc++){
      for(yloc = 0; yloc < land_y; yloc++){
        free(land[xloc][yloc]);   
      }
      free(land[xloc]);        
    }
    free(land); 

    ind_number = (int) paras[101];
    for(row = 0; row < ind_number; row++){
      free(pests[row]);
    }
    free(pests);
    
    free(paras);
    
    return(OUTPUT);
}
/* ===========================================================================*/

