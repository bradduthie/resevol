#include "utilities.h"

/* =============================================================================
 * Get the densities of pests on the landscape
 *     pests: The array holding the pests's information
 *     land:   The landscape array to be checked
 *     paras:  The paras vector that holds global information
 *     thresholds:  The vector defining pest density thresholds for pesticides
 *     delay:       The vector defining delay in pesticide application
 *     delay_count: The temporary vector indicating the delay countdown
 * ========================================================================== */
void pest_dense(double **pests, double ***land, double *paras, 
                    double *thresholds, int *delay, int *delay_count){
    
    int N, i, j, xdim, ydim, owner, own_layer, farms, xloc, yloc, xcol, ycol;
    int *pest_count, *farm_size;
    double the_size, the_pests, pest_density;
    
    xcol      = (int) paras[1];
    ycol      = (int) paras[2];
    N         = (int) paras[101];
    xdim      = (int) paras[103];
    ydim      = (int) paras[104];
    farms     = (int) paras[142];
    own_layer = (int) paras[155];
    
    farm_size    = (int *) malloc(farms * sizeof(int));
    pest_count   = (int *) malloc(farms * sizeof(int));
    
    for(i = 0; i < farms; i++){
        farm_size[i]    = 0;
        pest_count[i]   = 0;
    }
    
    for(i = 0; i < xdim; i++){
        for(j = 0; j < ydim; j++){
            owner  = (int) land[i][j][own_layer] - 1;
            farm_size[owner]++;
        }
    }
    
    for(i = 0; i < N; i++){
        xloc   = (int) pests[i][xcol];
        yloc   = (int) pests[i][ycol];
        owner  = (int) land[xloc][yloc][own_layer] - 1;
        pest_count[owner]++;
    }
    
    for(i = 0; i < farms; i++){
        the_size     = (double) farm_size[i];
        the_pests    = (double) pest_count[i];
        pest_density = the_pests / the_size;
        
        if(pest_density > thresholds[i] && delay_count[i] > 0){
            delay_count[i]--;
        }
        if(pest_density < thresholds[i] && delay_count[i] < delay[i]){
            delay_count[i] = delay[i];
        }
        
    }
    
    free(pest_count);
    free(farm_size);
}




