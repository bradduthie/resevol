#' Initialise individuals and simulate farming
#' 
#' Initialises a new set of individuals and then simulates farming over time
#'@param mine_output The output from mine_gmatrix
#'@param N Number of individuals to be initialised
#'@param xdim Horizontal dimensions of the landscape
#'@param ydim Vertical dimensions of the landscape
#'@param repro Type of reproduction allowed: "asexual", "sexual", and
#'  "biparental". Note that if repro > 0, this causes a diploid genome.
#'@param neutral_loci The number of neutral loci individuals have
#'@param max_age The maximum age of an individual
#'@param min_age_move The minimum age at which an individual can move
#'@param max_age_move The maximum age at which an individual can move
#'@param min_age_reproduce The minimum age which an individual can reproduce
#'@param max_age_reproduce The maximum age which an individual can reproduce
#'@param min_age_feed The minimum age at which an individual feeds
#'@param max_age_feed The maximum age at which an individual feeds
#'@param food_consume The amount of food consumed during feeding
#'@param rand_age Initialise individuals with a random age
#'@param move_distance Maximum cells moved in one bout of movement
#'@param food_needed_surv Food needed to survive (if over min_age_feed)
#'@param pesticide_tolerated_surv Pesticide tolerated by individual
#'@param food_needed_repr Food needed to reproduce 1 offspring
#'@param pesticide_tolerated_repr Pesticide tolerated to allow reproduction
#'@param reproduction_type Poisson reproduction ("lambda") vs "food_based"
#'@param mating_distance Distance in cells within which mate is available
#'@param lambda_value individual value for poisson reproduction
#'@param movement_bouts Number of bouts of movement per time step
#'@param selfing If sexual reproduction, is selfing allowed?
#'@param feed_while_moving Do individuals feed after each movement bout?
#'@param mortality_type Type of mortality (currently only one option)
#'@param age_food_threshold Age at which food threshold is enacted
#'@param age_pesticide_threshold Age at which pesticide threshold is enacted
#'@param farms How many farms should there be on the landscape?
#'@param time_steps Time steps in the simulation
#'@param mutation_pr Probability of a loci mutating
#'@param crossover_pr Probability of crossover at homologous loci
#'@param mutation_type Type of mutation used
#'@param net_mu_layers Layers of the network allowed to mutate
#'@param net_mu_dir  Layers mutate from loci to (1) or traits back (0)
#'@param mutation_direction Is mutation directional (unlikely to need)
#'@param crop_rotation_type None (1) or random (2) rotation of crop type
#'@param crop_rotation_time How frequently are the crops rotated?
#'@param pesticide_rotation_type None (1) or random (2) rotation of pesticide
#'@param pesticide_rotation_time How frequently are the pesticides rotated?
#'@param crop_per_cell How much crop is put on a single cell?
#'@param pesticide_per_cell How much pesticide is put on a single cell?
#'@param crop_sd What is the standard deviation of crop on a cell?
#'@param pesticide_sd What is the standard deviation of pesticide on a cell?
#'@param crop_min What is the minimum crop amount allowed per cell?
#'@param crop_max What is the maximum crop amount allowed per cell?
#'@param pesticide_min What is the minimum pesticide amount allowed per cell?
#'@param pesticide_max What is the maximum pesticide amount allowed per cell?
#'@param crop_number How many crops exist on the landscape?
#'@param pesticide_number How many pesticides are applied on the landscape?
#'@param print_inds Should the full list of individuals be printed? (CAREFUL)
#'@param print_gens Should a summary of each time step be printed?
#'@param print_last Should the last time step of individuals be printed?
#'@param K_on_birth Is there a carrying capacity applied on newborns?
#'@param pesticide_start What time step should pesticide start being applied?
#'@param immigration_rate Mean number of immigrants per time step
#'@return prints a simulation file and output
#'@useDynLib helicoverpa
#'@importFrom stats rnorm rpois runif
#'@export
run_farm_sim <- function(mine_output,
                         N = 1000, 
                         xdim = 100, 
                         ydim = 100, 
                         repro = "sexual", 
                         neutral_loci = 1000, 
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
                         mortality_type = 0,
                         age_food_threshold = 0,
                         age_pesticide_threshold = 0,
                         farms = 4,
                         time_steps = 100, 
                         mutation_pr = 0,
                         crossover_pr = 0, 
                         mutation_type = 0, 
                         net_mu_layers = 0,
                         net_mu_dir = 0, 
                         mutation_direction = 0,
                         crop_rotation_type = 2,
                         crop_rotation_time = 1,
                         pesticide_rotation_type = 2,
                         pesticide_rotation_time = 1,
                         crop_per_cell = 1,
                         pesticide_per_cell = 1,
                         crop_sd = 0,
                         pesticide_sd = 0,
                         crop_min = 0,
                         crop_max = 1000,
                         pesticide_min = 0,
                         pesticide_max = 1000,
                         crop_number = 2,
                         pesticide_number = 1,
                         print_inds = FALSE, 
                         print_gens = TRUE,
                         print_last = TRUE,
                         K_on_birth = 1000000,
                         pesticide_start = 0,
                         immigration_rate = 0){
  
    land <- make_landscape(rows = ydim, cols = xdim, depth = 21, farms = farms);
  
    move_distance_T             <- check_is_trait(move_distance);
    food_needed_surv_T          <- check_is_trait(food_needed_surv);
    pesticide_tolerated_surv_T  <- check_is_trait(pesticide_tolerated_surv);
    food_needed_repr_T          <- check_is_trait(food_needed_repr);
    pesticide_tolerated_repr_T  <- check_is_trait(pesticide_tolerated_repr);
    mating_distance_T           <- check_is_trait(mating_distance);
    lambda_value_T              <- check_is_trait(lambda_value);
    movement_bouts_T            <- check_is_trait(movement_bouts);
    
    food_consume_T <- rep(x = 0, times = length(food_consume));
    for(i in 1:length(food_consume)){
          food_consume_T[i] <- check_is_trait(food_consume[i]);
    }
    
    pesticide_consume_T <- rep(x = 0, times = length(pesticide_consume));
    for(i in 1:length(pesticide_consume)){
          pesticide_consume_T[i] <- check_is_trait(pesticide_consume[i]);
    }
    
    move_dist <- move_distance;
    if(move_distance_T == TRUE){
        move_dist <- 0;
    }
    
    food_n_surv <- food_needed_surv;
    if(food_needed_surv_T == TRUE){
        food_n_surv <- 0;
    }
    
    pest_t_surv <- pesticide_tolerated_surv;
    if(pesticide_tolerated_surv_T == TRUE){
        pest_t_surv <- 0;
    }
    
    food_n_repr <- food_needed_repr;
    if(food_needed_repr_T == TRUE){
        food_n_repr <- 0;
    }
    
    pest_t_repr <- pesticide_tolerated_repr;
    if(pesticide_tolerated_repr_T == TRUE){
        pest_t_repr <- 0;
    }
    
    mate_dist <- mating_distance;
    if(mating_distance_T == TRUE){
        mate_dist <- 0;
    }
    
    lamb_val <- lambda_value;
    if(lambda_value_T == TRUE){
        lamb_val <- 0;
    }
    
    move_bout <- movement_bouts;
    if(movement_bouts_T == TRUE){
        move_bout <- 0;
    }
    
    if(N < 5){
      stop("Need to start with at least 5 pests");
    }
    
    food_consume <- as.list(food_consume);
    food_cons    <- NULL;
    for(i in 1:length(food_consume)){
        if(food_consume_T[i] == TRUE){
            food_cons[[i]]    <- as.numeric(0);
        }else{
            food_cons[[i]]    <- as.numeric(food_consume[[i]]);
            food_consume[[i]] <- as.numeric(food_consume[[i]]);
        }
    }
    
    pesticide_consume <- as.list(pesticide_consume);
    pest_cons         <- NULL;
    for(i in 1:length(pesticide_consume)){
        if(pesticide_consume_T[i] == TRUE){
            pest_cons[[i]] <- as.numeric(0);
        }else{
            pest_cons[[i]]         <- as.numeric(pesticide_consume[[i]]);
            pesticide_consume[[i]] <- as.numeric(pesticide_consume[[i]]);
        }
    }
    
    pest <- initialise_inds(mine_output              = mine_output, 
                            N                        = N, 
                            xdim                     = xdim, 
                            ydim                     = ydim, 
                            repro                    = repro, 
                            neutral_loci             = neutral_loci, 
                            max_age                  = max_age,
                            min_age_move             = min_age_move, 
                            max_age_move             = max_age_move,
                            min_age_reproduce        = min_age_reproduce, 
                            max_age_reproduce        = max_age_reproduce, 
                            min_age_feed             = min_age_feed, 
                            max_age_feed             = max_age_feed,
                            food_consume             = food_cons, 
                            pesticide_consume        = pest_cons,
                            rand_age                 = rand_age, 
                            move_distance            = move_dist, 
                            food_needed_surv         = food_n_surv, 
                            pesticide_tolerated_surv = pest_t_surv,
                            food_needed_repr         = food_n_repr,
                            pesticide_tolerated_repr = pest_t_repr,
                            reproduction_type        = reproduction_type,
                            mating_distance          = mate_dist,
                            lambda_value             = lamb_val,
                            movement_bouts           = move_bout,
                            selfing                  = selfing,
                            feed_while_moving        = feed_while_moving,
                            mortality_type           = mortality_type,
                            age_food_threshold       = age_food_threshold,
                            age_pesticide_threshold  = age_pesticide_threshold);
    
    sim_results <- sim_crops(pests                    = pest, 
                             land                     = land,
                             time_steps               = time_steps, 
                             mutation_pr              = mutation_pr,
                             crossover_pr             = crossover_pr, 
                             mutation_type            = mutation_type,
                             net_mu_layers            = net_mu_layers, 
                             net_mu_dir               = net_mu_dir,
                             mutation_direction       = mutation_direction,
                             crop_rotation_type       = crop_rotation_type,
                             crop_rotation_time       = crop_rotation_time,
                             pesticide_rotation_type  = pesticide_rotation_type,
                             pesticide_rotation_time  = pesticide_rotation_time,
                             crop_per_cell            = crop_per_cell,
                             pesticide_per_cell       = pesticide_per_cell,
                             crop_sd                  = crop_sd,
                             pesticide_sd             = pesticide_sd,
                             crop_min                 = crop_min,
                             crop_max                 = crop_max,
                             pesticide_min            = pesticide_min,
                             pesticide_max            = pesticide_max,
                             crop_number              = crop_number,
                             pesticide_number         = pesticide_number,
                             print_inds               = print_inds, 
                             print_gens               = print_gens,
                             print_last               = print_last,
                             K_on_birth               = K_on_birth,
                             move_distance            = move_distance,
                             food_needed_surv         = food_needed_surv,
                             pesticide_tolerated_surv = pest_t_surv,
                             food_needed_repr         = food_needed_repr,
                             pesticide_tolerated_repr = pest_t_repr,
                             mating_distance          = mating_distance,
                             food_consume             = food_consume,
                             pesticide_consume        = pesticide_consume,
                             lambda_value             = lambda_value,
                             movement_bouts           = movement_bouts,
                             pesticide_start          = pesticide_start,
                             immigration_rate         = immigration_rate);
  
    return(sim_results);
}

sim_crops <- function(pests, 
                      land, 
                      time_steps = 100, 
                      mutation_pr = 0,
                      crossover_pr = 0, 
                      mutation_type = 0, 
                      net_mu_layers = 0,
                      net_mu_dir = 0, 
                      mutation_direction = 0,
                      crop_rotation_type = 2,
                      crop_rotation_time = 1,
                      pesticide_rotation_type = 2,
                      pesticide_rotation_time = 1,
                      crop_per_cell = 1,
                      pesticide_per_cell = 1,
                      crop_sd = 0,
                      pesticide_sd = 0,
                      crop_min = 0,
                      crop_max = 1000,
                      pesticide_min = 0,
                      pesticide_max = 1000,
                      crop_number = 2,
                      pesticide_number = 1,
                      print_inds = FALSE, 
                      print_gens = TRUE,
                      print_last = TRUE,
                      K_on_birth = 0,
                      move_distance = 1,
                      food_needed_surv = 0.25,
                      pesticide_tolerated_surv = 0.1,
                      food_needed_repr = 0,
                      pesticide_tolerated_repr = 0,
                      mating_distance = 0,
                      food_consume = 0.25,
                      pesticide_consume = 0.1,
                      lambda_value = 1,
                      movement_bouts = 1,
                      pesticide_start = 0,
                      immigration_rate = 0
                      ){
  
  N    <- dim(pests)[1];
  W    <- dim(pests)[2];
  X    <- dim(land)[2];
  Y    <- dim(land)[1];
  Z    <- dim(land)[3];
  smu  <- 1/sqrt(2); # SD of mutated loci values for sexual individuals
  amu  <- 1;         # SD of mutated loci values for asexual individuals
  ts   <- time_steps;
  mupr <- mutation_pr;
  crpr <- crossover_pr;
  mutp <- mutation_type;
  netm <- net_mu_layers;
  mudr <- mutation_direction;
  netd <- net_mu_dir;
  crty <- crop_rotation_type;
  crti <- crop_rotation_time;
  prty <- pesticide_rotation_type;
  prti <- pesticide_rotation_time;
  crpc <- crop_per_cell;
  pepc <- pesticide_per_cell;
  crsd <- crop_sd;
  pssd <- pesticide_sd;
  crmn <- crop_min;
  crmx <- crop_max;
  pemn <- pesticide_min;
  pemx <- pesticide_max;
  crpN <- crop_number;
  pesN <- pesticide_number;
  prin <- as.numeric(print_inds);
  prgn <- as.numeric(print_gens);
  plst <- as.numeric(print_last);
  konb <- K_on_birth;
  pdst <- pesticide_start;
  immi <- immigration_rate;
  
  paras  <- c( 0.0,   # 00) pests column for ID
               1.0,   # 01) pests column for xloc
               2.0,   # 02) pests column for yloc
               3.0,   # 03) pests column for age
               4.0,   # 04) pests column for sex
               5.0,   # 05) pests column for movement distance
               6.0,   # 06) pests column for mother ID
               7.0,   # 07) pests column for father ID
               8.0,   # 08) pests column for mother row
               9.0,   # 09) pests column for father row
              10.0,   # 10) pests column for offspring produced
              11.0,   # 11) pests column for loci number
              12.0,   # 12) pests column for trait number
              13.0,   # 13) pests column for network layers
              14.0,   # 14) pests column for food consumed
              15.0,   # 15) pests column for pesticide consumed
              16.0,   # 16) pests column for food needed survival
              17.0,   # 17) pests column for pesticide tolerated survival
              18.0,   # 18) pests column for food needed reproduction
              19.0,   # 19) pests column for pesticide tolerated reproduction
              20.0,   # 20) pests column tagging 1
              21.0,   # 21) pests column tagging 2
              22.0,   # 22) pests column tagging 3
              23.0,   # 23) pests column for reproduction type
              24.0,   # 24) pests column for mating distance
              25.0,   # 25) pests column for reproduction parameter
              26.0,   # 26) pests column for whether selfing is allowed
              27.0,   # 27) pests column for mate accessed
              28.0,   # 28) pests column for ploidy
              29.0,   # 29) pests column for neutral alleles number
              30.0,   # 30) pests column for movement bouts in a time step
              31.0,   # 31) pests column for min age of movement
              32.0,   # 32) pests column for max age of movement
              33.0,   # 33) pests column for min age of feeding
              34.0,   # 34) pests column for max age of feeding
              35.0,   # 35) pests column for min age of mating and reproduction
              36.0,   # 36) pests column for max age of mating and reproduction
              37.0,   # 37) pests column for food 1 consumption
              38.0,   # 38) pests column for food 2 consumption
              39.0,   # 39) pests column for food 3 consumption
              40.0,   # 40) pests column for food 4 consumption 
              41.0,   # 41) pests column for food 5 consumption
              42.0,   # 42) pests column for food 6 consumption
              43.0,   # 43) pests column for food 7 consumption
              44.0,   # 44) pests column for food 8 consumption
              45.0,   # 45) pests column for food 9 consumption
              46.0,   # 46) pests column for food 10 consumption
              47.0,   # 47) pests column for biopesticide 1 consumption
              48.0,   # 48) pests column for biopesticide 2 consumption
              49.0,   # 49) pests column for biopesticide 3 consumption
              50.0,   # 50) pests column for biopesticide 4 consumption
              51.0,   # 51) pests column for biopesticide 5 consumption
              52.0,   # 52) pests column for biopesticide 6 consumption
              53.0,   # 53) pests column for biopesticide 7 consumption
              54.0,   # 54) pests column for biopesticide 8 consumption
              55.0,   # 55) pests column for biopesticide 9 consumption
              56.0,   # 56) pests column for biopesticide 10 consumption
              57.0,   # 57) pests column for eating during bout
              58.0,   # 58) pests column for food 1 consumed
              59.0,   # 59) pests column for food 2 consumed
              60.0,   # 60) pests column for food 3 consumed
              61.0,   # 61) pests column for food 4 consumed
              62.0,   # 62) pests column for food 5 consumed
              63.0,   # 63) pests column for food 6 consumed
              64.0,   # 64) pests column for food 7 consumed
              65.0,   # 65) pests column for food 8 consumed
              66.0,   # 66) pests column for food 9 consumed
              67.0,   # 67) pests column for food 10 consumed
              68.0,   # 68) pests column for biopesticide 1 consumed
              69.0,   # 69) pests column for biopesticide 2 consumed
              70.0,   # 70) pests column for biopesticide 3 consumed
              71.0,   # 71) pests column for biopesticide 4 consumed
              72.0,   # 72) pests column for biopesticide 5 consumed
              73.0,   # 73) pests column for biopesticide 6 consumed
              74.0,   # 74) pests column for biopesticide 7 consumed
              75.0,   # 75) pests column for biopesticide 8 consumed
              76.0,   # 76) pests column for biopesticide 9 consumed
              77.0,   # 77) pests column for biopesticide 10 consumed
              78.0,   # 78) pests column for pesticide consumption during bout
              79.0,   # 79) pests column for mortality type
              80.0,   # 80) pests column for maximum age
              81.0,   # 81) pests column for mortality has occurred
              82.0,   # 82) pests column for age of food threshold enacted
              83.0,   # 83) pests column for age of pesticide threshold enacted
              84.0,   # 84) pests column for inbreeding coefficient
              85.0,   # 85) pests column for lamba adjustment
              86.0,   # 86)
              87.0,   # 87)
              88.0,   # 88)
              89.0,   # 89)
              90.0,   # 90)
              91.0,   # 91)
              92.0,   # 92)
              93.0,   # 93)
              94.0,   # 94)
              95.0,   # 95)
              96.0,   # 96)
              97.0,   # 97)
              98.0,   # 98)
              99.0,   # 99)
              100.0,  # 100)
              N,      # 101) Number of rows in the pest array
              0,      # 102) Torus landscape
              X,      # 103) x dimension of the landscape
              Y,      # 104) y dimension of the landscape
              Z,      # 105) z dimension (depth) of the landscape
              0,      # 106) Dynamic count of total offspring
              W,      # 107) Number of cols in the pest array
              N,      # 108) Highest ID of an individual
              100,    # 109) Column where the traits start
              crpr,   # 110) Crossover probability for sexual reproduction
              mutp,   # 111) Mutation type (0 = new allele; 1 = vary existing)
              mupr,   # 112) Mutation rate
              netm,   # 113) Network layers that can mutate 
              mudr,   # 114) Mutation direction
              amu,    # 115) Mutation SD (asexual)
              smu,    # 116) Mutation SD (sexual)
              netd,   # 117) Layers mutate from loci to (1) or traits back (0)
              1,      # 118) Land layer where the food 1 is located
              2,      # 119) Land layer where the food 2 is located
              3,      # 120) Land layer where the food 3 is located
              4,      # 121) Land layer where the food 4 is located
              5,      # 122) Land layer where the food 5 is located
              6,      # 123) Land layer where the food 6 is located
              7,      # 124) Land layer where the food 7 is located
              8,      # 125) Land layer where the food 8 is located
              9,      # 126) Land layer where the food 9 is located
              10,     # 127) Land layer where the food 10 is located
              11,     # 128) Land layer where the pesticide 1 is located
              12,     # 129) Land layer where the pesticide 2 is located
              13,     # 130) Land layer where the pesticide 3 is located
              14,     # 131) Land layer where the pesticide 4 is located
              15,     # 132) Land layer where the pesticide 5 is located
              16,     # 133) Land layer where the pesticide 6 is located
              17,     # 134) Land layer where the pesticide 7 is located
              18,     # 135) Land layer where the pesticide 8 is located
              19,     # 136) Land layer where the pesticide 9 is located
              20,     # 137) Land layer where the pesticide 10 is located
              N,      # 138) Number of individuals following mortality
              N,      # 139) Number of individuals in the next time step
              ts,     # 140) Total number of time steps to run
              0,      # 141) Extinction has occurred
              crty,   # 142) Type of crop rotation
              crti,   # 143) Time steps between crop rotation
              crpc,   # 144) Crop production per landscape cell
              0,      # 145) Type of crop production (UNUSED)
              crmn,   # 146) Minimum crop production per landscape cell
              crmx,   # 147) Maximum crop production per landscape cell
              prty,   # 148) Type of biopesticide rotation
              crti,   # 149) Time steps between biopesticide rotation
              pepc,   # 150) Biopesticide amount per landscape cell
              0,      # 151) Type of biopesticide application (UNUSED)
              pssd,   # 152) StDev in biopesticide per landscape cell
              pemn,   # 153) Minimum biopesticide per landscape cell
              pemx,   # 154) Maximum biopesticide per landscape cell
              0,      # 155) Landscape layer where land owner is located
              crpN,   # 156) Number of crops produced
              pesN,   # 157) Number of biopesticides used
              10,     # 158) Maximum possible crop types
              10,     # 159) Maximum possible biopesticide types
              0,      # 160) Proportion land left fallow (UNUSED)
              0,      # 161) Proportion land with no biopesticide (UNUSED)
              crsd,   # 162) StDev in crop production per landscape cell
              0,      # 163) Time taken for simulation (in seconds)
              prin,   # 164) Print individual level data
              prgn,   # 165) Print time step and N in the console
              plst,   # 166) Print last individuals in simulation
              konb,   # 167) Maximum number of births allowed in time step
              pdst,   # 168) When does the pesticide use start?
              immi,   # 169) Average number of immigrants per time step
              0       # 170) Realised number of immigrants
              );

  paras <- substitute_traits(paras, move_distance, food_needed_surv,
                             pesticide_tolerated_surv, food_needed_repr,
                             pesticide_tolerated_repr, mating_distance,
                             food_consume, pesticide_consume, lambda_value,
                             movement_bouts);
  
  if(is.array(pests) == FALSE){
    stop("ERROR: pests must be a 2D array.");
  }
  if(is.array(land) == FALSE){
    stop("ERROR: land must be a 3D array.");
  }
  
  SIM_RESULTS        <- run_farming_sim(pests, land, paras);

  return(SIM_RESULTS);
}

run_farming_sim <- function(IND, LAND, PARAS){
  .Call("sim_farming", IND, LAND, PARAS);
}


substitute_traits <- function(paras, move_distance, food_needed_surv,
                              pesticide_tolerated_surv, food_needed_repr,
                              pesticide_tolerated_repr, mating_distance,
                              food_consume, pesticide_consume, lambda_value,
                              movement_bouts){
  
    Tst <- paras[110] - 1;
  
    move_distance_T             <- check_is_trait(move_distance);
    food_needed_surv_T          <- check_is_trait(food_needed_surv);
    pesticide_tolerated_surv_T  <- check_is_trait(pesticide_tolerated_surv);
    food_needed_repr_T          <- check_is_trait(food_needed_repr);
    pesticide_tolerated_repr_T  <- check_is_trait(pesticide_tolerated_repr);
    mating_distance_T           <- check_is_trait(mating_distance);
    lambda_value_T              <- check_is_trait(lambda_value);
    movement_bouts_T            <- check_is_trait(movement_bouts);
    
    food_consume_T <- rep(x = 0, times = length(food_consume));
    for(i in 1:length(food_consume)){
        food_consume_T[i] <- check_is_trait(food_consume[[i]]);
    }
    pesticide_consume_T <- rep(x = 0, times = length(pesticide_consume));
    for(i in 1:length(pesticide_consume)){
        pesticide_consume_T[i] <- check_is_trait(pesticide_consume[[i]]);
    }
    
    # This series of ifs inserts the trait columns into the paras vector
    if(move_distance_T == TRUE){
        move_distance_N <- get_trait_number(move_distance) + Tst;
        paras[6]        <- move_distance_N;
    }
    
    if(food_needed_surv_T == TRUE){
        food_needed_surv_N <- get_trait_number(food_needed_surv) + Tst;
        paras[17]          <- food_needed_surv_N;
    }
    
    if(pesticide_tolerated_surv_T == TRUE){
        pesti_tol_surv_N <- get_trait_number(pesticide_tolerated_surv) + Tst;
        paras[18]          <- pesti_tol_surv_N;
    }
    
    if(food_needed_repr_T == TRUE){
        food_needed_repr_N <- get_trait_number(food_needed_repr) + Tst;
        paras[19]          <- food_needed_repr_N;
    }
    
    if(pesticide_tolerated_repr_T == TRUE){
        pesti_tol_repr_N <- get_trait_number(pesticide_tolerated_repr) + Tst;
        paras[20]        <- pesti_tol_repr_N;
    }
    
    if(mating_distance_T == TRUE){
        mating_distance_N <- get_trait_number(mating_distance) + Tst;
        paras[25]         <- mating_distance_N;
    }
    
    if(lambda_value_T == TRUE){
        lambda_value_N <- get_trait_number(lambda_value) + Tst;
        paras[26]      <- lambda_value_N;
    }
    
    if(movement_bouts_T == TRUE){
        movement_bouts_N <- get_trait_number(movement_bouts) + Tst;
        paras[31]        <- movement_bouts_N;
    }
    
    for(i in 1:length(food_consume)){
        if(food_consume_T[i] == TRUE){
            food_consume_N <- get_trait_number(food_consume[[i]]) + Tst;
            paras[37 + i]  <- food_consume_N;
        }
    }
    
    for(i in 1:length(pesticide_consume)){
        if(pesticide_consume_T[i] == TRUE){
            pesticide_cons_T <- get_trait_number(pesticide_consume[[i]]) + Tst;
            paras[47 + i]    <- pesticide_cons_T;
        }
    }
    
    return(paras);
}

# Checks to see if something has been assigned as a trait
check_is_trait <- function(val){
  is_trait <- FALSE;
  val_char <- is.character(val);
  if(val_char == TRUE){
      val_splt <- strsplit(val, split = "")[[1]][1];
      if(val_splt == "T"){
          is_trait <- TRUE;
      }
  }
  return(is_trait);
}

# Get the number portion of trait (e.g., 20 for "T20")
get_trait_number <- function(trait){
  trait_n  <- 1;
  val_splt <- strsplit(trait, split = "")[[1]];
  val_numb <- val_splt[-1];
  val_char <- collapse_vals(val_numb);
  trait_n  <- as.numeric(val_char);
  return(trait_n);
}

# Function to combine character elements
collapse_vals <- function(...) {
  paste(..., sep = "", collapse = "");
}
