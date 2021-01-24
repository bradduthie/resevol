#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>



void matrix_multiply(double **m1, double **m2, int m1_rows, int m1_cols,
                     int m2_rows, int m2_cols, double **m_out){
    
    /* Add break if non-conformable arrays? */
    int row, col, ele;
    double val; 

    for(row = 0; row < m1_rows; row++){
        for(col = 0; col < m2_cols; col++){
            val = 0;
            for(ele = 0; ele < m1_cols; ele++){
                val += (m1[row][ele] * m2[ele][col]);
            }
            m_out[row][col] = val;
        }
    }
}


/* =============================================================================
 * MAIN RESOURCE FUNCTION:
 * ===========================================================================*/

/* =============================================================================
 * This is the outer function for mining the g-matrices
 *  Inputs include:
 *      PARAS: Nothing yet, but will hold the paramters of interest
 * ===========================================================================*/
SEXP mine_gmatrix(SEXP PARAS){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int    i, j, k, val;
    int    row;
    int    col;
    int    vec_pos;
    int    protected_n;    /* Number of protected R objects */
    double len_PARAS;      /* Length of the parameters vector */
    double *paras;         /* parameter values read into R */
    double *paras_ptr;     /* pointer to the parameters read into C */
    double *paras_ptr_new; /* Pointer to new paras (interface R and C) */
    
    int loci;
    int traits;
    int layers;
    
    double **loci_layer_one;
    double **net_temp;
    double ***net;


    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */

    protected_n = 0;
    
    PROTECT( PARAS = AS_NUMERIC(PARAS) );
    protected_n++;
    paras_ptr = REAL(PARAS);
    
    len_PARAS = GET_LENGTH(PARAS);


    /* The C code for the model itself falls under here */
    /* ====================================================================== */
    
    paras   = malloc(len_PARAS * sizeof(double *));
    vec_pos = 0;
    for(i = 0; i < len_PARAS; i++){
        paras[i] = paras_ptr[vec_pos];
        vec_pos++;
    } /* The parameters vector is now copied into C */
    
    /* Do the biology here now */
    /* ====================================================================== */
    
    loci   = 2;
    traits = 2;
    layers = 3;
    
    loci_layer_one  = malloc(traits * sizeof(double *));
    for(row = 0; row < traits; row++){
        loci_layer_one[row] = malloc(loci * sizeof(double));   
    } 
    
    net   = malloc(layers * sizeof(double *));
    for(k = 0; k < layers; k++){
        net[k] = malloc(traits * sizeof(double *));
        for(i = 0; i < traits; i++){
            net[k][i] = malloc(traits * sizeof(double));   
        }
    } 

    net_temp = malloc(traits * sizeof(double *));
    for(row = 0; row < traits; row++){
        net_temp[row] = malloc(traits * sizeof(double));   
    } 
    
    for(row = 0; row < traits; row++){
        for(col = 0; col < traits; col++){
            net_temp[row][col] = 0;
        }
    }
    

    
    val = 1;
    for(row = 0; row < traits; row++){
        for(col = 0; col < loci; col++){
            loci_layer_one[row][col] = val; /* rnorm(0, 1);  */
            val++;
        }
    }
    
    val = 1; 
    for(k = 0; k < layers; k++){
        for(i = 0; i < traits; i++){
            for(j = 0; j < traits; j++){
                net[k][i][j] = val;
                val++;
            }
        }
    }    
    
    
    
    
    for(k = 0; k < layers; k++){
        printf("\n\n");
        for(i = 0; i < traits; i++){
            for(j = 0; j < traits; j++){
                printf("%f\t", net[k][i][j]);
            }
            printf("\n");
        }
    }

    printf("\n\n ****************************************** \n\n");
    
    
    for(k = 1; k < layers; k++){
        matrix_multiply(net[k-1], net[k], traits, traits, traits, traits, 
                        net_temp);
        for(i = 0; i < traits; i++){
            for(j = 0; j < traits; j++){
                net[k][i][j] = net_temp[i][j];
            }
        }
    }
    
    
    printf("\n\n");
    for(i = 0; i < traits; i++){
        for(j = 0; j < traits; j++){
            printf("%f\t", net_temp[i][j]);
        }
        printf("\n");
    }

     
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
    
    SEXP GOUT;
    GOUT = PROTECT( allocVector(VECSXP, 1) );
    protected_n++;
    SET_VECTOR_ELT(GOUT, 0, PARAMETERS_NEW);
    
    UNPROTECT(protected_n);
    
    /* Free all of the allocated memory used in arrays */
    for(k = 0; k < layers; k++){
        for(i = 0; i < traits; i++){
            free(net[k][i]);
        }
        free(net[k]);        
    }
    free(net); 
    
    for(row = 0; row < traits; row++){
        free(loci_layer_one[row]);
    }
    free(loci_layer_one);
    
    for(row = 0; row < traits; row++){
        free(net_temp[row]);
    }
    free(net_temp);
    
    
    free(paras);


    return(GOUT); 
}
/* ===========================================================================*/

