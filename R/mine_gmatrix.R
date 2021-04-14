#' Mine G-matrices
#'
#' Mine networks for establishing the link between genome and g-matrix.
#'
#'@param paras Doesn't matter what this is yet, it will run.
#'@param gmatrix G-matrix that the evolutionary algorithm will match
#'@return A set of values that will produce a desired G-matrix
#'@export
mine_gmatrix <- function(paras = c(2, 2), gmatrix){
    if(dim(gmatrix)[1] != dim(gmatrix)[2]){
        stop("ERROR: 'gmatrix' needs to be a square matrix");
    }
    GOUT     <- run_mine_gmatrix(PARAS = paras, GMATRIX_c = gmatrix);
    return(GOUT); # Returns the values to produce the desired g-matrix
}

run_mine_gmatrix <- function(PARAS, GMATRIX_c){
    .Call("mine_gmatrix", PARAS, GMATRIX_c);
}