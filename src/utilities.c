#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* =============================================================================
 * This is a generic function to multiply two matrices together
 *     m1:      The first matrix to be multiplied
 *     m2:      The second matrix to be multiplied
 *     m1_rows: Number of rows in matrix m1
 *     m1_cols: Number of columns in matrix m1
 *     m2_rows: Number of rows in matrix m2
 *     m2_cols: Number of columns in matrix m2
 *     m_out:   The output product matrix
 * ========================================================================== */
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
 * This function multiplies the square matrices that make up the layers of an
 * array (net) to produce a two dimensional matrix (net_out) product.
 *     traits:  Traits of an individual; also matrix rows and columns
 *     layers:  Layers of the network (i.e., how many matrices in the array)
 *     net:     The 3D array in which layers are square matrices
 *     net_out: The output matrix that is the product of the array layers
 * ========================================================================== */
void sum_network_layers(int traits, int layers, double ***net, 
                        double **net_out){
    
    int i, j, k;
    double ***net_temp;
    
    net_temp = (double ***) malloc(layers * sizeof(double **));
    for(k = 0; k < layers; k++){
        net_temp[k] = (double **) malloc(traits * sizeof(double *));
        for(i = 0; i < traits; i++){
            net_temp[k][i] = (double *)  malloc(traits * sizeof(double));   
        }
    }
    for(k = 0; k < layers; k++){
        for(i = 0; i < traits; i++){
            for(j = 0; j < traits; j++){
                net_temp[k][i][j] = net[k][i][j];
            }
        }
    }
    
    for(k = 1; k < layers; k++){
        matrix_multiply(net_temp[k-1], net_temp[k], traits, traits, traits, 
                        traits, net_out);
        if(k < layers){
            for(i = 0; i < traits; i++){
                for(j = 0; j < traits; j++){
                    net_temp[k][i][j] = net_out[i][j]; 
                }
            }
        }
    }
    
    for(k = 0; k < layers; k++){
        for(i = 0; i < traits; i++){
            free(net_temp[k][i]);
        }
        free(net_temp[k]);        
    }
    free(net_temp); 
}

/* =============================================================================
 * This function sets all elements in a matrix to a value of zero:
 *     rows: Rows within the matrix
 *     cols: Columns within the matrix
 *     mat:  The matrix itself
 * ========================================================================== */
void matrix_zeros(int rows, int cols, double **mat){
    
    int row, col;
    
    for(row = 0; row < rows; row++){
        for(col = 0; col < cols; col++){
            mat[row][col] = 0;
        }
    }
}

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
    land_type = (int) paras[102];
    xdim      = (int) paras[103];
    ydim      = (int) paras[104];
    
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
    
    if(from == to){
        rand_value = (int) from;
    }else{
        do{
            rand_value = (int) floor( runif(from, (to + 1) ) );
        }while( rand_value == (to + 1) );
    }
    
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

/* =============================================================================
 * Sample an element from a probability vector
 *     vec: The vector
 *     len: The length of the vector
 * ========================================================================== */
int sample_pr_vector(double *vec, int len){
    
    int i;
    double u, S;
    
    u = runif(0, 1);
    S = 0.0;

    for(i = 0; i < len; i++){
        S += vec[i];
        if(S >= u){
            break;
        }
    }
    
    
    return 0;
}
