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
    
    if(repro != "asexual" & repro != "sexual" & repro != "biparental"){
      stop("ERROR: Must specify 'repro' as asexual, sexual, or biparental.")
    }
    if(repro == "sexual" | repro == "biparental"){
      inds      <- build_sexual(mine_output);
    }else{
      inds <- build_asexual(mine_output);
    }
    
    inds[, 1] <- 1:N; # Sample ID
    inds[, 2] <- sample(x = 1:xdim, size = N, replace = TRUE); # xloc
    inds[, 3] <- sample(x = 1:ydim, size = N, replace = TRUE); # yloc
    inds[, 4] <- 0; # Age
    if(repro == "asexual"){
      inds[,5] <- 0;
    }
    if(repro == "sexual"){
      inds[,5] <- 1;
    }
    if(repro == "biparental"){
      inds[,5] <- sample(x = 2:3, size = N, replace = TRUE);
    }
    inds[, 6]  <-  0; # Mate distance
    inds[, 7]  <-  1; # Movement distance
    inds[, 8]  <- -1; # Mother ID
    inds[, 9]  <- -1; # Father ID
    inds[, 10] <- -1; # Mother row
    inds[, 11] <- -1; # Father row
    inds[, 12] <-  0; # Offspring produced
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
  
  ind_neutral_cols  <- rnorm(n = (N * neutral_loci), mean = 0, sd = 1);
  
  inds       <- matrix(data = 0, nrow = N, ncol = ind_end_col);
  
  inds[, trait_start_col:(layers_start_col - 1)]  <- ind_traits_mat;
  inds[, layers_start_col:(loci_start_col - 1)]   <- net_layer_cols;
  inds[, loci_start_col:(genome_start_col - 1)]   <- ind_loci_mat;
  inds[, genome_start_col:genome_end_col]         <- ind_genome_cols;
  inds[, (genome_end_col + 1):ind_end_col]        <- ind_neutral_cols;
  
  return(inds);
}


build_sexual <- function(mine_output){
  
  loci       <- mine_output[[1]][1];
  layers     <- mine_output[[1]][2];
  traits     <- dim(mine_output[[2]])[1];
  
  ind_loci_vals   <- rnorm(n = 2 * N * loci, mean = 0, sd = 1/sqrt(2));
  ind_loci_mat    <- matrix(data = ind_loci_vals, nrow = N, ncol = 2 * loci);
  loci_1_cols     <- 1:loci;
  loci_2_cols     <- ((loci+1):(2*loci));
  ind_loci_addi   <- ind_loci_mat[,loci_1_cols] + ind_loci_mat[,loci_2_cols];
  ind_traits_mat  <- ind_loci_addi %*% mine_output[[5]];
  
  genome          <- 0.5 * mine_output[[7]];
  ind_first_cols  <- matrix(data = 0, nrow = N, ncol = 20);
  
  trait_start_col   <- dim(ind_first_cols)[2] + 1;
  layers_start_col  <- trait_start_col + traits;
  loci_start_col    <- layers_start_col + layers + 1;
  genome_start_col  <- loci_start_col + (2 * loci);
  genome_end_col    <- genome_start_col + length(genome);
  dip_geno_end_col  <- genome_start_col + (2 * length(genome)) - 1;
  ind_end_col       <- dip_geno_end_col + neutral_loci;
  
  net_start_col     <- genome_start_col + ((2 * loci) * traits);
  net_layer_sep     <- seq(from = net_start_col, to = genome_end_col, 
                           by = (traits * traits));
  net_separators    <- c(genome_start_col, net_layer_sep, dip_geno_end_col);
  
  net_layer_cols    <- matrix(data = net_separators, nrow = N, 
                              ncol = length(net_separators), byrow = TRUE);
  ind_genome_cols   <- matrix(data = genome, nrow = N, 
                              ncol = 2 * length(genome), byrow = TRUE);
  
  ind_neutral_cols  <- rnorm(n = (N * neutral_loci), mean = 0, sd = 1);
  
  inds       <- matrix(data = 0, nrow = N, ncol = ind_end_col);
  
  inds[, trait_start_col:(layers_start_col - 1)]  <- ind_traits_mat;
  inds[, layers_start_col:(loci_start_col - 1)]   <- net_layer_cols;
  inds[, loci_start_col:(genome_start_col - 1)]   <- ind_loci_mat;
  inds[, genome_start_col:dip_geno_end_col]       <- ind_genome_cols;
  inds[, (dip_geno_end_col + 1):ind_end_col]      <- ind_neutral_cols;
  
  return(inds);
}

