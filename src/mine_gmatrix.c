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
    int    i, j, k;
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
    
    loci   = 4;
    traits = 4;
    layers = 3;
    
    loci_layer_one  = malloc(traits * sizeof(double *));
    for(row = 0; row < traits; row++){
        loci_layer_one[row] = malloc(loci * sizeof(double));   
    } 
    
    net   = malloc(traits * sizeof(double *));
    for(i = 0; i < traits; i++){
        net[i] = malloc(traits * sizeof(double *));
        for(j = 0; j < traits; j++){
            net[i][j] = malloc(layers * sizeof(double));   
        }
    } 

    
    
    
    
    
    
    for(row = 0; row < traits; row++){
        for(col = 0; col < loci; col++){
            loci_layer_one[row][col] = rnorm(0, 1); 
        }
    }
    
    
    for(i = 0; i < traits; i++){
        for(j = 0; j < traits; j++){
            for(k = 0; k < layers; k++){
                net[i][j][k] = rnorm(0, 1); 
            }
        }
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
    for(i = 0; i < traits; i++){
        for(j = 0; j < traits; j++){
            free(net[i][j]);   
        }
        free(net[i]);        
    }
    free(net); 
    
    for(row = 0; row < traits; row++){
        free(loci_layer_one[row]);
    }
    free(loci_layer_one);
    
    
    free(paras);


    return(GOUT); 
}
/* ===========================================================================*/

