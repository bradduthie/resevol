#' Initialise individuals
#'
#' Initialise new individuals into the IBM
#'
#'@param mine_output The output from mine_gmatrix
#'@param N Number of individuals to be initialised
#'@param xdim Horizontal dimensions of the landscape
#'@param ydim Vertical dimensions of the landscape
#'@return A set of values that will produce a desired G-matrix
#'@export
initialise_inds <- function(mine_output, N = 1000, xdim = 100, ydim = 100){
    
    loci       <- mine_output[[1]][1];
    layers     <- mine_output[[1]][2];
    traits     <- dim(mine_output[[2]])[1];
    
    ind_Lv     <- rnorm(n = N * loci, mean = 0, sd = 1);
    ind_Lm     <- matrix(data = ind_Lv, nrow = N, ncol = loci);
    ind_Tr     <- ind_Lm %*% mine_output[[5]];
    
    ind_ini    <- matrix(data = 0, nrow = N, ncol = 20);
    
    trait_st   <- 21;
    loci_st    <- trait_st + traits;
    layers_st  <- loci_st + loci;
    genome_st  <- layers_st + layers;
    genome_end <- genome_st + length(mine_output[[7]]);
    
    net_st     <- genome_st + (loci * traits);
    nets       <- seq(from = net_st, to = genome_end, by = (traits * traits));
    
    # Going to bind as follows:
    # 1. 20 columns that are empty (starts at 1)
    # 2. Whatever number of traits there are (starts at 21)
    # 3. Whatever number of loci there are (starts at 21 + traits)
    # 4. Whatever number of layers there are (starts at 21 + traits + loci)
    # 5. The rest of the genome (starts at 21 + traits + loci + layers)
    # Genome ends at 21 + traits + loci + layers + length(mine_output[[7]])
    
    # So traits start in column 
    
}