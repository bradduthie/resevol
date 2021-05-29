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
    
    ind_loci_vals   <- rnorm(n = N * loci, mean = 0, sd = 1);
    ind_loci_mat    <- matrix(data = ind_loci_vals, nrow = N, ncol = loci);
    ind_traits_mat  <- ind_loci_mat %*% mine_output[[5]];
    genome          <- mine_output[[7]];
    ind_first_cols  <- matrix(data = 0, nrow = N, ncol = 20);
    
    trait_start_col   <- dim(ind_first_cols)[2] + 1;
    layers_start_col  <- trait_start_col + traits;
    loci_start_col    <- layers_start_col + layers + 1;
    genome_start_col  <- loci_start_col + loci;
    genome_end_col    <- genome_start_col + length(genome) - 1;
  
    net_start_col     <- genome_start_col + (loci * traits);
    net_layer_sep     <- seq(from = net_start_col, to = genome_end_col, 
                             by = (traits * traits));
    net_separators    <- c(genome_start_col, net_layer_sep);
    
    net_layer_cols    <- matrix(data = net_separators, nrow = N, 
                                ncol = length(net_separators), byrow = TRUE);
    ind_genome_cols   <- matrix(data = genome, nrow = N, ncol = length(genome),
                                byrow = TRUE);
    
    inds       <- matrix(data = 0, nrow = N, ncol = genome_end_col);

    inds[, trait_start_col:(layers_start_col - 1)]  <- ind_traits_mat;
    inds[, layers_start_col:(loci_start_col - 1)]   <- net_layer_cols;
    inds[, loci_start_col:(genome_start_col - 1)]   <- ind_loci_mat;
    inds[, genome_start_col:genome_end_col]         <- ind_genome_cols;
    
    # Going to bind as follows:
    # 1. 20 columns that are empty (starts at 1)
    # 2. Whatever number of traits there are (starts at 21)
    # 3. Whatever number of loci there are (starts at 21 + traits)
    # 4. Whatever number of layers there are (starts at 21 + traits + loci)
    # 5. The rest of the genome (starts at 21 + traits + loci + layers)
    # Genome ends at 21 + traits + loci + layers + length(mine_output[[7]])
    
    # So traits start in column 
    
}
