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
#include "immigration.h"

/* =============================================================================
 * This is the outer function for simulating farming and pesticide resistance
 *  Inputs include:
 *      IND:     Array with individual pest data
 *      LAND:    Three dimensional array of landscape
 *      PARAS:   Nothing yet, but will hold the parameters of interest
 *      CROT:    Crop rotation matrix
 *      PROT:    Pesticide rotation matrix
 *      CINIT:   Initial crop choice for each farmer
 *      PINIT:   Initial pesticide choice for each farmer
 * ===========================================================================*/
SEXP sim_farming(SEXP IND, SEXP LAND, SEXP PARAS, SEXP CROT, SEXP PROT,
                 SEXP CINIT, SEXP PINIT){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int    i, ts;
    int    row, col;
    int    xloc, yloc, zloc;
    int    land_z, land_y, land_x;
    int    CROT_d1, CROT_d2, PROT_d1, PROT_d2;
    int    CINIT_d1, CINIT_d2, PINIT_d1, PINIT_d2;
    int    vec_pos;
    int    time_steps;
    int    ind_number;
    int    ind_traits;
    int    offspring_number; /* New number of individuals post reproduction */
    int    new_total_N;      /* Total number of individuals after a time step */
    int    surviving_N;      /* Surviving individuals after a time step */
    int    immigrants;       /* Immigrants in a time step */
    int    protected_n;      /* Number of protected R objects */
    int    len_PARAS;        /* Length of the parameters vector */
    int    print_gen;        /* Should the generations be printed */
    int    get_stats;        /* Should print a CSV with statistics */
    int    *dim_IND;         /* Dimensions of the individual array */
    int    *dim_LAND;        /* Dimensions of the landscape */
    int    *dim_CROT;        /* Dimensions of the crop transition matrix */
    int    *dim_PROT;        /* Dimensions of the pesticide transition matrix */
    int    *dim_CINIT;       /* Dimensions of the crop initialisation matrix */
    int    *dim_PINIT;       /* Dimensions of the pesticide init matrix */
  
    double *imm_sample;
    double *paras_ptr;
    double *IND_ptr;
    double *LAND_ptr;
    double *CROT_ptr;
    double *PROT_ptr;
    double *CINIT_ptr;
    double *PINIT_ptr;
    double *paras;
    double **pests;        /* The pests array */
    double **offspring;    /* The offspring of pests within a time step */
    double **new_pests;    /* The pest array at the end of a time step */
    double ***land;        /* The landscape array */
    double **C_change;     /* Transition array for crops */
    double **P_change;     /* Transition array for pesticides */
    double **C_init;       /* Initialisation array for crops */
    double **P_init;       /* Initialisation array for pesticides */
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
    
    PROTECT( CROT = AS_NUMERIC(CROT) );
    protected_n++;
    CROT_ptr = REAL(CROT);
    
    PROTECT( PROT = AS_NUMERIC(PROT) );
    protected_n++;
    PROT_ptr = REAL(PROT);
    
    PROTECT( CINIT = AS_NUMERIC(CINIT) );
    protected_n++;
    CINIT_ptr = REAL(CINIT);
    
    PROTECT( PINIT = AS_NUMERIC(PINIT) );
    protected_n++;
    PINIT_ptr = REAL(PINIT);
    
    len_PARAS   = GET_LENGTH(PARAS);
    dim_IND     = INTEGER( GET_DIM(IND) );
    dim_LAND    = INTEGER( GET_DIM(LAND) );
    dim_CROT    = INTEGER( GET_DIM(CROT) );
    dim_PROT    = INTEGER( GET_DIM(PROT) );
    dim_CINIT   = INTEGER( GET_DIM(CINIT) );
    dim_PINIT   = INTEGER( GET_DIM(PINIT) );
    
    /* The C code for the model itself falls under here */
    /* ====================================================================== */
    
    paras   = (double *) malloc(len_PARAS * sizeof(double));
    vec_pos = 0;
    for(i = 0; i < len_PARAS; i++){
        paras[i] = paras_ptr[vec_pos];
        vec_pos++; 
    } /* The parameters vector is now copied into C */

    /* Code below remakes the IND matrix for easier use */
    ind_number = dim_IND[0];
    ind_traits = dim_IND[1];
    
    pests  = (double **) malloc(ind_number * sizeof(double *));
    for(row = 0; row < ind_number; row++){
        pests[row] = (double *) malloc(ind_traits * sizeof(double));   
    } 
    vec_pos = 0;
    for(col = 0; col < ind_traits; col++){
        for(row = 0; row < ind_number; row++){
            pests[row][col] = IND_ptr[vec_pos]; 
            vec_pos++;
        }
    }
     
    /* Code below remakes the LAND array for easier use */
    land_x = dim_LAND[1];
    land_y = dim_LAND[0];
    land_z = dim_LAND[2];
    
    land   = (double ***) malloc(land_x * sizeof(double **));
    for(xloc = 0; xloc < land_x; xloc++){
      land[xloc] = (double **) malloc(land_y * sizeof(double *));
      for(yloc = 0; yloc < land_y; yloc++){
        land[xloc][yloc] = (double *) malloc(land_z * sizeof(double));   
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
    
    
    /* Code below remakes the crop rotation matrix for easier use */
    CROT_d1   = dim_CROT[0];
    CROT_d2   = dim_CROT[1];
    C_change  = (double **) malloc(CROT_d1 * sizeof(double *));
    for(row = 0; row < CROT_d1; row++){
        C_change[row] = (double *) malloc(CROT_d2 * sizeof(double));   
    } 
    vec_pos = 0;
    for(col = 0; col < CROT_d2; col++){
        for(row = 0; row < CROT_d1; row++){
            C_change[row][col] = CROT_ptr[vec_pos]; 
            vec_pos++;
        }
    }
    
    /* Code below remakes the pesticide rotation matrix for easier use */
    PROT_d1   = dim_PROT[0];
    PROT_d2   = dim_PROT[1];
    P_change  = (double **) malloc(PROT_d1 * sizeof(double *));
    for(row = 0; row < PROT_d1; row++){
        P_change[row] = (double *) malloc(PROT_d2 * sizeof(double));   
    } 
    vec_pos = 0;
    for(col = 0; col < PROT_d2; col++){
        for(row = 0; row < PROT_d1; row++){
            P_change[row][col] = PROT_ptr[vec_pos]; 
            vec_pos++;
        }
    }
    
    /* Code below remakes the crop initialisation matrix for easier use */
    CINIT_d1   = dim_CINIT[0];
    CINIT_d2   = dim_CINIT[1];
    C_init  = (double **) malloc(CINIT_d1 * sizeof(double *));
    for(row = 0; row < CINIT_d1; row++){
        C_init[row] = (double *) malloc(CINIT_d2 * sizeof(double));   
    } 
    vec_pos = 0;
    for(col = 0; col < CINIT_d2; col++){
        for(row = 0; row < CINIT_d1; row++){
            C_init[row][col] = CINIT_ptr[vec_pos]; 
            vec_pos++;
        }
    }
  
    /* Code below remakes the pesticide initialisation matrix for easier use */
    PINIT_d1   = dim_PINIT[0];
    PINIT_d2   = dim_PINIT[1];
    P_init  = (double **) malloc(PINIT_d1 * sizeof(double *));
    for(row = 0; row < PINIT_d1; row++){
        P_init[row] = (double *) malloc(PINIT_d2 * sizeof(double));   
    } 
    vec_pos = 0;
    for(col = 0; col < PINIT_d2; col++){
        for(row = 0; row < PINIT_d1; row++){
            P_init[row][col] = PINIT_ptr[vec_pos]; 
            vec_pos++;
        }
    }
    
    /* Do the biology here now */
    /* ====================================================================== */
    imm_sample = (double *) malloc(ind_traits * sizeof(double));
    for(col = 0; col < ind_traits; col++){
        imm_sample[col] = pests[0][col];
    }
    
    print_gen  = (int) paras[165];
    get_stats  = (int) paras[172];
    time_steps = (int) paras[140];
    ts         = 0;
    
    while(ts < time_steps){
 
        land_change(land, paras, ts, C_init, C_change); 
      
        age_pests(pests, paras); 
        
        feeding(pests, paras, land); 
        
        pesticide_consumed(pests, paras, land); 

        movement(pests, paras, land); 
        
        calculate_offspring(pests, paras);
    
        offspring_number = (int) paras[106]; 
        offspring  = (double **) malloc(offspring_number * sizeof(double *));
        for(row = 0; row < offspring_number; row++){
            offspring[row] = (double *) malloc(ind_traits * sizeof(double));   
        } 

        if(offspring_number > 0){
            make_offspring(pests, offspring, paras);
        }

        apply_mortality(pests, paras);
    
        print_all_pests(pests, paras, ts);
        if(get_stats > 0){
            population_statistics(pests, paras, ts);
        }
        
        immigrants = get_immigrant_number(paras);
        paras[170] = (double) immigrants;
    
        surviving_N = (int) paras[138];
        new_total_N = offspring_number + surviving_N + immigrants;
        paras[139]  = (double) new_total_N; 

        if(new_total_N < 6){
            for(row = 0; row < offspring_number; row++){
                free(offspring[row]);
            }
            free(offspring);
            paras[141] = 1;
            break;
        }
        
        new_pests   = (double **) malloc(new_total_N * sizeof(double *));
        for(row = 0; row < new_total_N; row++){
            new_pests[row] = (double *) malloc(ind_traits * sizeof(double));   
        } 
        
        fill_new_pests(pests, offspring, new_pests, paras, imm_sample);
        
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
        pests      = (double **) malloc(new_total_N * sizeof(double *));
        for(row = 0; row < new_total_N; row++){
            pests[row] = (double *) malloc(ind_traits * sizeof(double));   
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
    for(row = 0; row < PINIT_d1; row++){
        free(P_init[row]);
    }
    free(P_init);
    
    for(row = 0; row < CINIT_d1; row++){
        free(C_init[row]);
    }
    free(C_init);
    
    for(row = 0; row < PROT_d1; row++){
        free(P_change[row]);
    }
    free(P_change);
    
    for(row = 0; row < CROT_d1; row++){
        free(C_change[row]);
    }
    free(C_change);
    
    free(imm_sample);
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

