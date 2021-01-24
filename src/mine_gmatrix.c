#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

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
    int    i;
    int    vec_pos;
    int    protected_n;    /* Number of protected R objects */
    double len_PARAS;      /* Length of the parameters vector */
    double *paras;         /* parameter values read into R */
    double *paras_ptr;     /* pointer to the parameters read into C */
    double *paras_ptr_new; /* Pointer to new paras (interface R and C) */
    

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
    vec_pos   = 0;
    for(i = 0; i < len_PARAS; i++){
        paras[i] = paras_ptr[vec_pos];
        vec_pos++;
    } /* The parameters vector is now copied into C */
    
    /* Do the biology here now */
    /* ====================================================================== */
    
    
    
    
    
    

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
    free(paras);

    return(GOUT); 
}
/* ===========================================================================*/

