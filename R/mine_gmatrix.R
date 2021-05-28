#' Mine G-matrices
#'
#' Mine networks for establishing the link between genome and g-matrix.
#'
#'@param loci The number of loci that individuals in the model will have
#'@param layers The number of layers in the network from loci to traits
#'@param indivs The number of individuals to test the covariance matrix
#'@param npsize The size of the network population in the evolutionary algorithm
#'@param mu_pr The probability of a network value to mutate
#'@param mu_sd The standard deviation of mutation effect size
#'@param max_gen The maximum number of generations of the evolutionary algorithm
#'@param pr_cross The probability of a crossover occurring for a network
#'@param sampleK Number of networks sampled to take part in tournament selection
#'@param chooseK Number of winners in tournament selection
#'@param term_cri Stress criteria (ln) for evolutionary algorithm terminating
#'@param sd_ini StDev of initialised networked values
#'@param use_cor Compare correlation matrix rather than the covariance matrix
#'@param gmatrix G-matrix that the evolutionary algorithm will match
#'@return A set of values that will produce a desired G-matrix
#'@export
mine_gmatrix <- function(loci     = 18, 
                         layers   = 6, 
                         indivs   = 1000,
                         npsize   = 2000, 
                         mu_pr    = 0.05, 
                         mu_sd    = 0.01,
                         max_gen  = 1000,
                         pr_cross = 0.05,
                         sampleK  = 40,
                         chooseK  = 4,
                         term_cri = -5.3,
                         sd_ini   = 0.1,
                         use_cor  = FALSE,
                         gmatrix){
    
    paras <- c(loci, layers, indivs, npsize, mu_pr, mu_sd, max_gen, pr_cross,
               sampleK, chooseK, term_cri, sd_ini, use_cor);
    
    if(loci < 2 | loci %% 1 > 0){
        stop("ERROR: 'loci' needs to be an integer value above 1.")
    }
    if(layers < 1 | layers %% 1 > 0){
        stop("ERROR: 'layers' needs to be an integer value above 0.")
    }
    if(indivs < 10 | indivs %% 1 > 0){
        stop("ERROR: 'indivs' needs to be an integer value above 9.")
    }
    if(npsize < 10 | npsize %% 1 > 0){
        stop("ERROR: 'npsize' needs to be an integer value above 9.")
    }
    if(mu_pr > 1 | mu_pr < 0){
        stop("ERROR: 'mu_pr' must be a value between 0 and 1.")
    }
    if(mu_sd <= 0){
        stop("ERROR: 'mu_sd' must be a value greater than 0.")
    }
    if(max_gen < 1 | max_gen %% 1 > 0){
        stop("ERROR: 'max_gen' needs to be an integer value above 0.")
    }
    if(pr_cross > 1 | pr_cross < 0){
        stop("ERROR: 'pr_cross' must be a value between 0 and 1.")
    }
    if(sampleK < 1 | sampleK %% 1 > 0){
        stop("ERROR: 'sampleK' needs to be an integer value above 0.")
    }
    if(chooseK < 1 | chooseK %% 1 > 0){
        stop("ERROR: 'chooseK' needs to be an integer value above 0.")
    }
    if(sd_ini <= 0){
        stop("ERROR: 'sd_ini' must be a value greater than 0.")
    }
    if(dim(gmatrix)[1] != dim(gmatrix)[2]){
        stop("ERROR: 'gmatrix' needs to be a square matrix");
    }
    GOUT     <- run_mine_gmatrix(PARAS = paras, GMATRIX_c = gmatrix);
    return(GOUT); # Returns the values to produce the desired g-matrix
}

run_mine_gmatrix <- function(PARAS, GMATRIX_c){
    .Call("mine_gmatrix", PARAS, GMATRIX_c);
}