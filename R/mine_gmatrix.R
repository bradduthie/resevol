#' Mine G-matrices
#'
#' Mine networks for establishing the link between genome and g-matrix.
#'
#'@param paras Doesn't matter what this is yet, it will run.
#'@return A set of values that will produce a desired G-matrix
#'@export
mine_gmatrix <- function(paras){
    GOUT     <- run_mine_gmatrix(PARAS = paras);
    return(GOUT); # Returns the values to produce the desired g-matrix
}

run_mine_gmatrix <- function(PARAS){
    .Call("mine_gmatrix", PARAS);
}