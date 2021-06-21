#'Initialise individuals
#'
#'Initialise new individuals into the IBM
#'
#'@param mine_output The output from mine_gmatrix
#'@param N Number of individuals to be initialised
#'@param xdim Horizontal dimensions of the landscape
#'@param ydim Vertical dimensions of the landscape
#'@param repro Type of reproduction allowed: "asexual", "sexual", and
#'  "biparental". Note that if repro > 0, this causes a diploid genome.
#'@param neutral_loci The number of neutral loci individuals have
#'@return A set of values that will produce a desired G-matrix
#'@export
initialise_inds <- function(mine_output, N = 1000, xdim = 100, ydim = 100, 
                            repro = "sexual", neutral_loci = 0){
    
    if(repro != "asexual" & repro != "sexual" & repro != "biparenta"){
      stop("ERROR: Must specify 'repro' as asexual, sexual, or biparental.")
    }
    if(repro == "sexual" | repro == "biparental"){
      inds <- "Not ready yet";
    }else{
      inds <- build_asexual(mine_output);
    }
    
    inds[, 1] <- 1:N;
    inds[, 2] <- sample(x = 1:xdim, size = N, replace = TRUE);
    inds[, 3] <- sample(x = 1:ydim, size = N, replace = TRUE);
    
    return(inds);
}


build_asexual <- function(mine_output){
  
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
  loci_start_col    <- layers_start_col + layers + 2;
  genome_start_col  <- loci_start_col + loci;
  genome_end_col    <- genome_start_col + length(genome) - 1;
  ind_end_col       <- genome_end_col + neutral_loci;
  
  net_start_col     <- genome_start_col + (loci * traits);
  net_layer_sep     <- seq(from = net_start_col, to = genome_end_col, 
                           by = (traits * traits));
  net_separators    <- c(genome_start_col, net_layer_sep, genome_end_col + 1);
  
  net_layer_cols    <- matrix(data = net_separators, nrow = N, 
                              ncol = length(net_separators), byrow = TRUE);
  ind_genome_cols   <- matrix(data = genome, nrow = N, ncol = length(genome),
                              byrow = TRUE);
  
  ind_neutral_cols  <- rnorm(n = (N * 1000), mean = 0, sd = 1);
  
  inds       <- matrix(data = 0, nrow = N, ncol = genome_end_col);
  
  inds[, trait_start_col:(layers_start_col - 1)]  <- ind_traits_mat;
  inds[, layers_start_col:(loci_start_col - 1)]   <- net_layer_cols;
  inds[, loci_start_col:(genome_start_col - 1)]   <- ind_loci_mat;
  inds[, genome_start_col:genome_end_col]         <- ind_genome_cols;
  
  return(inds);
}




