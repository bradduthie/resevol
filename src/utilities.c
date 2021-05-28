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