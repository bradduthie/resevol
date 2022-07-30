#'Initialise individuals
#'
#'Initialise new individuals into the IBM. This function is generally not needed
#'because it is run inside the run_farm_sim function to generate new individuals
#'for simulations. To initialise individuals with this function, it is necessary
#'to set the mine_output argument to output from the mine_gmatrix function. This
#'output includes all of the information necessary to build individuals with
#'genomes that produce traits that covary in a pre-specified way. The arguments
#'of this function include addition information for building the individual
#'array, which is a two-dimensional array in which each individual occupies a 
#'row, and each column specifies a character of the individual (including all
#'genome loci). See vignettes for a more detailed explanation. 
#'
#'@param mine_output The output from mine_gmatrix
#'@param N Number of individuals to be initialised
#'@param xdim Horizontal dimensions of the landscape
#'@param ydim Vertical dimensions of the landscape
#'@param repro Type of reproduction allowed: "asexual", "sexual", and
#'  "biparental". Note that if repro != "asexual", this causes a diploid genome.
#'@param neutral_loci The number of neutral loci individuals have (must be > 0)
#'@param max_age The maximum age of an individual
#'@param min_age_move The minimum age at which an individual can move
#'@param max_age_move The maximum age at which an individual can move
#'@param min_age_reproduce The minimum age which an individual can reproduce
#'@param max_age_reproduce The maximum age which an individual can reproduce
#'@param min_age_feed The minimum age at which an individual feeds
#'@param max_age_feed The maximum age at which an individual feeds
#'@param food_consume The amount of food consumed during feeding
#'@param pesticide_consume Amount of pesticide consumed while on a cell
#'@param rand_age Initialise individuals with a random age (TRUE/FALSE)
#'@param move_distance Maximum cells moved in one bout of movement
#'@param food_needed_surv Food needed to survive (if over min_age_feed)
#'@param pesticide_tolerated_surv Pesticide tolerated by individual
#'@param food_needed_repr Food needed to reproduce 1 offspring
#'@param pesticide_tolerated_repr Pesticide tolerated to allow reproduction
#'@param reproduction_type Poisson reproduction ("lambda") vs "food_based"
#'@param mating_distance Distance in cells within which mate is available
#'@param lambda_value individual value for poisson reproduction
#'@param movement_bouts Number of bouts of movement per time step
#'@param selfing If sexual reproduction, is selfing allowed? (TRUE/FALSE)
#'@param feed_while_moving Do individuals feed after each movement bout?
#'@param pesticide_while_moving Individuals consume pesticide after move bout?
#'@param mortality_type Type of mortality (currently only one option)
#'@param age_food_threshold Age at which food threshold is enacted
#'@param age_pesticide_threshold Age at which pesticide threshold is enacted
#'@param metabolism The amount of consumed food lost each time step
#'@param baseline_metabolism A fixed baseline rate added to 'metabolism'+
#'@param min_age_metabolism The minimum age affected by metabolism
#'@param max_age_metabolism The maximum age affected by metabolism
#'@param trait_means The means of the custom pest traits
#'@return A two-dimensional array of individuals for simulation
#'@examples
#'gmt       <- matrix(data = 0, nrow = 2, ncol = 2);
#'diag(gmt) <- 1;
#'mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 2, indivs = 100, 
#'                          npsize = 100, max_gen = 4, prnt_out = FALSE);
#'inds      <- initialise_inds(mine_output = mg, N = 40, repro = "asexual");
#'@importFrom stats rnorm
#'@export
initialise_inds <- function(mine_output, 
                            N = 1000, 
                            xdim = 100, 
                            ydim = 100, 
                            repro = "sexual", 
                            neutral_loci = 10, 
                            max_age = 9,
                            min_age_move = 0, 
                            max_age_move = 9,
                            min_age_reproduce = 0, 
                            max_age_reproduce = 9, 
                            min_age_feed = 0, 
                            max_age_feed = 9,
                            food_consume = 0.25, 
                            pesticide_consume = 0.1,
                            rand_age = FALSE, 
                            move_distance = 1, 
                            food_needed_surv = 0.25, 
                            pesticide_tolerated_surv = 0.1,
                            food_needed_repr = 0,
                            pesticide_tolerated_repr = 0,
                            reproduction_type = "lambda",
                            mating_distance = 1,
                            lambda_value = 1,
                            movement_bouts = 1,
                            selfing = TRUE,
                            feed_while_moving = FALSE,
                            pesticide_while_moving = FALSE,
                            mortality_type = 0,
                            age_food_threshold = NA,
                            age_pesticide_threshold = NA,
                            metabolism = 0,
                            baseline_metabolism = 0,
                            min_age_metabolism = 1,
                            max_age_metabolism = 9,
                            trait_means = NULL){
  
  food      <- rep(x = 0, times = 10);
  pesticide <- rep(x = 0, times = 10);
  
  sp_food   <- length(food_consume);
  sp_pesti  <- length(pesticide_consume);
  
  food[1:sp_food]       <- food_consume;
  pesticide[1:sp_pesti] <- pesticide_consume;
  
  if(is.na(age_food_threshold) == TRUE){
      age_food_threshold <- 0;
  }
  
  if(is.na(age_pesticide_threshold) == TRUE){
    age_pesticide_threshold <- 0;
  }
  
  if(N < 2){
    stop("ERROR: Must initialise with at least two individuals.");
  }
  
  if(neutral_loci < 10){
    stop("ERROR: Must initialise with at least 10 neutral loci.");
  }
  
  if(repro != "asexual" & repro != "sexual" & repro != "biparental"){
    stop("ERROR: Must specify 'repro' as asexual, sexual, or biparental.");
  }
  if(repro == "sexual" | repro == "biparental"){
    inds <- build_sexual(mine_output, N, neutral_loci, trait_means);
  }else{
    inds <- build_asexual(mine_output, N, neutral_loci, trait_means);
  }
  
  inds[, 1] <- 1:N; # Sample ID
  inds[, 2] <- sample(x = 0:(xdim - 1), size = N, replace = TRUE); # xloc
  inds[, 3] <- sample(x = 0:(ydim - 1), size = N, replace = TRUE); # yloc
  if(rand_age == FALSE){
      inds[, 4] <- 0; # Age
  }else{
      inds[, 4] <- sample(x = 0:max_age, size = N, replace = TRUE);
  }
  if(repro == "asexual"){
    inds[, 5]  <- 0;
    inds[, 29] <- 1; # Ploidy
    inds[, 30] <- neutral_loci;
  }
  if(repro == "sexual"){
    inds[,5]   <- 1;
    inds[, 29] <- 2;
    inds[, 30] <- neutral_loci;
  }
  if(repro == "biparental"){
    inds[,5]   <- sample(x = 2:3, size = N, replace = TRUE);
    inds[, 29] <- 2;
    inds[, 30] <- neutral_loci;
  }
  inds[, 6]   <-  move_distance; # Movement distance
  inds[, 7]   <- -1; # Mother ID
  inds[, 8]   <- -1; # Father ID
  inds[, 9]   <- -1; # Mother row
  inds[, 10]  <- -1; # Father row
  inds[, 11]  <-  0; # Offspring produced
  inds[, 12]  <-  mine_output[[1]][1]; # loci;
  inds[, 13]  <-  dim(mine_output[[2]])[1]; # traits;
  inds[, 14]  <-  mine_output[[1]][2]; # layers;
  inds[, 17]  <-  food_needed_surv;
  inds[, 18]  <-  pesticide_tolerated_surv;
  inds[, 19]  <-  food_needed_repr;
  inds[, 20]  <-  pesticide_tolerated_repr;
  if(reproduction_type == "lambda"){
    inds[, 24] <- 0;
  }
  if(reproduction_type == "food_based"){
    inds[, 24] <- 1;
  }
  inds[, 25]  <-  mating_distance; # Mate distance requirement
  inds[, 26]  <-  lambda_value;    # Reproduction parameter
  inds[, 27]  <-  selfing;
  inds[, 31]  <-  movement_bouts; # Movement bouts
  inds[, 32]  <-  min_age_move;      # Min age of movement
  inds[, 33]  <-  max_age_move;      # Max age of movement
  inds[, 34]  <-  min_age_feed;      # Min age of feeding
  inds[, 35]  <-  max_age_feed;      # Max age of feeding
  inds[, 36]  <-  min_age_reproduce; # Min age of mating and reproduction
  inds[, 37]  <-  max_age_reproduce; # Max age of mating and reproduction
  inds[, 38]  <-  food[[1]];
  inds[, 39]  <-  food[[2]];
  inds[, 40]  <-  food[[3]];
  inds[, 41]  <-  food[[4]];
  inds[, 42]  <-  food[[5]];
  inds[, 43]  <-  food[[6]];
  inds[, 44]  <-  food[[7]];
  inds[, 45]  <-  food[[8]];
  inds[, 46]  <-  food[[9]];
  inds[, 47]  <-  food[[10]];
  inds[, 48]  <-  pesticide[[1]];
  inds[, 49]  <-  pesticide[[2]];
  inds[, 50]  <-  pesticide[[3]];
  inds[, 51]  <-  pesticide[[4]];
  inds[, 52]  <-  pesticide[[5]];
  inds[, 53]  <-  pesticide[[6]];
  inds[, 54]  <-  pesticide[[7]];
  inds[, 55]  <-  pesticide[[8]];
  inds[, 56]  <-  pesticide[[9]];
  inds[, 57]  <-  pesticide[[10]];
  inds[, 58]  <-  feed_while_moving; # Do not eat on a bout
  inds[, 79]  <-  pesticide_while_moving;
  inds[, 80]  <-  mortality_type;
  inds[, 81]  <-  max_age;
  inds[, 83]  <-  age_food_threshold;
  inds[, 84]  <-  age_pesticide_threshold;
  inds[, 87]  <-  metabolism;
  inds[, 88]  <-  baseline_metabolism;
  inds[, 89]  <-  min_age_metabolism;
  inds[, 90]  <-  max_age_metabolism;
  inds[, 91]  <-  0;
  inds[, 92]  <-  0;
  inds[, 93]  <-  0;
  inds[, 94]  <-  0;
  inds[, 95]  <-  0;
  inds[, 96]  <-  0;
  inds[, 97]  <-  0;
  inds[, 98]  <-  0;
  inds[, 99]  <-  0;
  inds[, 100] <-  0;
  
  if(is.null(trait_means) == FALSE){
      for(i in 1:length(trait_means)){
          inds[, 90 + i] <- trait_means[i];
      }
  }
  
  return(inds);
}


build_asexual <- function(mine_output, N, neutral_loci, trait_means){
  
  loci       <- mine_output[[1]][1];
  layers     <- mine_output[[1]][2];
  traits     <- dim(mine_output[[2]])[1];
  
  ind_loci_vals   <- rnorm(n = N * loci, mean = 0, sd = 1);
  ind_loci_mat    <- matrix(data = ind_loci_vals, nrow = N, ncol = loci);
  ind_traits_mat  <- ind_loci_mat %*% mine_output[[5]];
  genome          <- mine_output[[7]];
  ind_first_cols  <- matrix(data = 0, nrow = N, ncol = 100);
  
  if(is.null(trait_means) == FALSE){
      if(length(trait_means) != traits){
          stop("ERROR: trait_means must be same length as total trait number.");
      }
      if(length(trait_means) > 10){
          stop("ERROR: More than 10 traits is not allowed.");
      }
      for(i in 1:traits){
          ind_traits_mat[, i] <- ind_traits_mat[, i] + trait_means[i];
      }
  }
  
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


build_sexual <- function(mine_output, N, neutral_loci, trait_means){
  
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
  ind_first_cols  <- matrix(data = 0, nrow = N, ncol = 100);
  
  if(is.null(trait_means) == FALSE){
      if(length(trait_means) != traits){
          stop("ERROR: trait_means must be same length as total trait number.");
      }
      if(length(trait_means) > 10){
          stop("ERROR: More than 10 traits is not allowed.");
      }
      for(i in 1:traits){
          ind_traits_mat[, i] <- ind_traits_mat[, i] + trait_means[i];
      }
  }
  
  trait_start_col   <- dim(ind_first_cols)[2] + 1;
  layers_start_col  <- trait_start_col + traits;
  loci_start_col    <- layers_start_col + layers + 3;
  genome_start_col  <- loci_start_col + (2 * loci);
  genome_end_col    <- genome_start_col + length(genome);
  dip_geno_end_col  <- genome_start_col + (2 * length(genome)) - 1;
  ind_end_col       <- dip_geno_end_col + (2 * neutral_loci);
  
  net_start_col     <- genome_start_col + (loci * traits);
  net_layer_sep     <- seq(from = net_start_col, to = genome_end_col, 
                           by = (traits * traits));
  net_separators    <- c(genome_start_col, net_layer_sep, dip_geno_end_col);
  
  net_layer_cols    <- matrix(data = net_separators, nrow = N, 
                              ncol = length(net_separators), byrow = TRUE);
  ind_genome_cols   <- matrix(data = genome, nrow = N, 
                              ncol = 2 * length(genome), byrow = TRUE);
  
  ind_neutral_cols  <- rnorm(n = (N * 2 * neutral_loci), mean = 0, sd = 1);
  
  inds       <- matrix(data = 0, nrow = N, ncol = ind_end_col);
  
  inds[, trait_start_col:(layers_start_col - 1)]  <- ind_traits_mat;
  inds[, layers_start_col:(loci_start_col -1)]    <- net_layer_cols;
  inds[, loci_start_col:(genome_start_col - 1)]   <- ind_loci_mat;
  inds[, genome_start_col:dip_geno_end_col]       <- ind_genome_cols;
  inds[, (dip_geno_end_col + 1):ind_end_col]      <- ind_neutral_cols;
  
  return(inds);
}
