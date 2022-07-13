#' Mine G-matrices
#'
#' Mine networks for establishing the link between genome and g-matrix. The 
#' output from this function is required to run individual-based simulations
#' in the rest of the package. The key input to this function, 'gmatrix', is a
#' (square) covariance matrix, with each row and column representing a trait for
#' the individual-based model. This function will run an evolutionary algorithm
#' to try to find a network that produces traits with the covariance structure
#' of gmatrix from a set of random standard normal values. The network from loci
#' values to trait values goes through a number of linked nodes to achieve this, 
#' and each generation tests the stress of the resulting network in terms of 
#' expected squared deviation of trait covariances from the input gmatrix. 
#' Simulations can take minutes to hours or longer, depending on parameters 
#' chosen and the number of traits. See vignettes for a more comprehensive
#' explanation for what this function is doing.
#'
#'@param loci The number of loci for an individual. Simulations can allow for 
#' both haploid and diploid individuals. Allele values at each loci affect trait
#' values through a network of intermediary nodes.
#'@param layers The number of hidden layers in the network linking loci to 
#' traits.
#'@param indivs The number of individuals initialised in each generation of the 
#' evolutionary algorithm to test among-individual trait correlations. 
#' Individuals are initialised with allele values drawn from a standard normal 
#' distribution.
#'@param npsize The size of the population of networks in each generation of the
#' evolutionary algorithm. Each network is a discrete individual in the 
#' population.
#'@param mu_pr The probability that a value in the network will mutate in a 
#' generation. Mutation events change the existing value by adding a new value 
#' drawn from a normal distribution with a mean of 0 and standard deviation of
#' mu_sd.
#'@param mu_sd The standard deviation of the random normal value mean centered 
#' at 0 that is added to the existing value of the network when a mutation event
#' occurs.
#'@param max_gen The maximum number of generations that the evolutionary 
#' algorithm is allowed to run before terminating (regardless of how well the 
#' evolved covariance structure matches the pre-specified gmatrix).
#'@param pr_cross The probability that a focal network in the population will 
#' initiate a crossover of a subset of its values with a randomly selected 
#' second network (note that any given network might therefore be included in 
#' more than one crossover event in a generation). The size of the subset is 
#' determined randomly.
#'@param sampleK During a round of selection, the number of random networks 
#' chosen to compete in a tournament. A single generation will include as many 
#' tournaments as necessary to create a new network population of size npsize.
#'@param chooseK During a round of selection tournament, the number of networks 
#' within the sampleK random subset of the tournament that have the highest 
#' fitness will be selected to populate the next generation of networks
#'@param term_cri The criteria for terminating the evolutionary algorithm. The 
#' algorithm will terminate if a network is found in which the mean squared 
#' deviation of the covariance matrix elements from gmatrix is less than 
#' exp(term_crit).
#'@param sd_ini The standard deviation of initialised network values at the 
#' start of the evolutionary algorithm. All network values are initialised by 
#' randomly sampling from a normal distribution with a mean of 0 and a 
#' standard deviation of sd_ini
#'@param use_cor Should the gmatrix be treated as a correlation matrix rather 
#' than a covariance matrix when calculating fitness?
#'@param prnt_out Should the function print out progress showing the stress for 
#' each generation
#'@param gmatrix The pre-specified trait covariance matrix. This will define
#' what the covariance will be between each trait when allele values are drawn 
#' from a standard normal distribution.
#'@return A list of eight elements that includes the following: (1) A vector of
#'input parameters, (2) the pre-specified covariance matrix, (3) matrix defining
#'the effects of loci values on the first layer of the network, (4) a three
#'dimensional array link the first network layer to trait values, (5) a matrix
#'of the marginal effect of each locus on each trait, (6) the mined covariance
#'structure, (7) all network values to be inserted into individual genomes, and
#'(8) the log stress of the mined matrix against the pre-specified matrix.
#'@examples
#'gmt       <- matrix(data = 0, nrow = 4, ncol = 4);
#'diag(gmt) <- 1;
#'mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 3, indivs = 100, 
#'                          npsize = 100, max_gen = 2, prnt_out = FALSE);
#'@useDynLib resevol
#'@importFrom stats rnorm rpois runif rbinom
#'@importFrom utils read.csv write.csv
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
                         prnt_out = TRUE,
                         gmatrix){
    
    paras <- c(loci, layers, indivs, npsize, mu_pr, mu_sd, max_gen, pr_cross,
               sampleK, chooseK, term_cri, sd_ini, use_cor, as.numeric(prnt_out)
               );
    
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