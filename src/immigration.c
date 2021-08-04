#include "utilities.h"

/* =============================================================================
 * Gets the number of immigrants that should be added
 *     paras:           The paras vector that holds global information
 * ========================================================================== */
int get_immigrant_number(double *paras){
    
    int realised_immigrants;
    double mean_immigrants;
    
    mean_immigrants     = paras[169];
    realised_immigrants = (int) rpois(mean_immigrants);
    
    return realised_immigrants;
}

