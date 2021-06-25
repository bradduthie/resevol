#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* =============================================================================
 * Returns a random integer value.
 *     from: The lowest integer to be randomly chosen
 *     to:   The hgihest value to be randomly chosen
 * ========================================================================== */
int get_rand_int(int from, int to){
    
    int rand_value;
    
    do{
        rand_value = (int) floor( runif(from, to) );
    }while(rand_value == to);
    
    return rand_value;
}

/* =============================================================================
 * Swap pointers to rewrite ARRAY_B into ARRAY_A for a an array of any dimension
 * ========================================================================== */
void swap_arrays(void **ARRAY_A, void **ARRAY_B){
    
    void *TEMP_ARRAY;
    
    TEMP_ARRAY = *ARRAY_A;
    *ARRAY_A   = *ARRAY_B;
    *ARRAY_B   = TEMP_ARRAY;
}

/* =============================================================================
 * Get the maximum value from a column of a 2D array
 *     array: The 2D array
 *     rows:  The number of rows in the array
 *     col:   The column of interest
 * ========================================================================== */
int max_in_col(double **array, int rows, int col){
    
    int i, max_val; 
    
    max_val = array[0][col];
    for(i = 1; i < rows; i++){
        if(array[i][col] > max_val){
            max_val = array[i][col];
        }
    }
    
    return max_val;
}

