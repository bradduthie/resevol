#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* =============================================================================
 * Checks to see if another individual is within a range of the focal individual
 *     from: The lowest integer to be randomly chosen
 *     to:   The highest value to be randomly chosen
 * ========================================================================== */
int is_in_range(double **pests, int focal, int other, double *paras, int range){
    
    int xcol, ycol, range_col, land_type;
    int other_x, other_y, xdim, ydim, in_range;
    int xdist, xdist_2, xdist_3, ydist, ydist_2, ydist_3, focal_x, focal_y;
    
    xcol      = (int) paras[1];
    ycol      = (int) paras[2];
    range_col = (int) paras[24];
    land_type = (int) paras[52];
    xdim      = (int) paras[53];
    ydim      = (int) paras[54];
    
    range     = pests[focal][range_col];
    focal_x   = pests[focal][xcol];
    focal_y   = pests[focal][ycol];
    
    other_x   = pests[other][xcol];
    other_y   = pests[other][ycol];
    xdist     = abs(focal_x - other_x);
    ydist     = abs(focal_y - other_y);
    
    in_range  = 0;
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
    if(xdist <= range && ydist <= range){
        in_range++;
    }
    
    return in_range;
}


/* =============================================================================
 * Returns a random integer value.
 *     from: The lowest integer to be randomly chosen
 *     to:   The highest value to be randomly chosen
 * ========================================================================== */
int get_rand_int(int from, int to){
    
    int rand_value;
    
    do{
        rand_value = (int) floor( runif(from, (to + 1)) );
    }while( rand_value == (to + 1) );
    
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

