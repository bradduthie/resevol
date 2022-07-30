#' Initialise individuals and simulate farming
#' 
#' Initialises a new set of individuals and then simulates farming over time.
#' This is the main function that runs individual-based simulations of crop and
#' pesticide use and the evolution of pesticide resistance over time. To run 
#' this function, output from the mine_gmatrix function is required to specify
#' the covariance structure of individual traits and individual genomes. The
#' arguments to this function are used to initialise a landscape with the 
#' make_landscape function and initialise individuals with the initialise_inds
#' function. After initialisation, the simulation continues for up to a set 
#' number of time steps (unless extinction occurs), and individuals on the
#' landscape feed, encounter pesticide, move, reproduce, and die depending upon
#' the arguments specified in this function. After a specified number of time
#' steps, the crop or pesticide applied to a landscape cell can also change. The
#' end result is an evolving population of individuals that express traits 
#' that can potentially affect fitness (e.g., food consumption, pesticide
#' consumption, movement). Population level statistics are calculated by 
#' default and printed to a CSV, but individual level data (which includes all 
#' individual characteristics in a large table) need to be turned on because 
#' files can become extremely large (use print_inds with extreme caution and
#' print_last with care).
#' 
#'@param mine_output The output from mine_gmatrix, which will be used to
#' initialise the genomes and traits of pests.
#'@param N The number of individuals that are initialised in a simulation. 
#' Individuals are initialised in a random location on the landscape, and at 
#' least two individuals are needed.
#'@param xdim The number of cells in the horizontal dimension of the landscape. 
#' This value must be an integer greater than two.
#'@param ydim The number of cells in the vertical dimension of the landscape. 
#' This value must be an integer greater than two.
#'@param repro The type of reproduction that individuals undergo in the 
#' simulation. There are three options: (1) "asexual," in which individuals 
#' reproduce clonally and offspring have haploid genomes and traits identical to
#' their mother with the potential for mutation; (2) "sexual," in which 
#' individuals are monoecious (both female and male) and offspring have diploid 
#' genomes with alleles inherited from both parents with mutation and 
#' recombination; (3) "biparental," in which individuals are dioecious (only 
#' female or male) and offspring have diploid genomes with alleles inherited 
#' from both parents with mutation and recombination.
#'@param neutral_loci The number of loci that are completely neutral (i.e., have
#' no effect on fitness). These loci can be used to monitor genetic drift or
#' calculate inbreeding coefficients.
#'@param max_age This is the maximum number of time steps that an individual can
#' survive. Individuals that are older than this age in a time step will always 
#' die.
#'@param min_age_move This is the minimum age at which an individual can move. 
#' Individuals below this age will always remain on their current cell.
#'@param max_age_move This is the maximum age at which an individual can move. 
#' Individuals above this age will always remain on their current cell.
#'@param min_age_reproduce This is the minimum age at which an individual can be
#' reproductively active. No individuals below this age will engage in any 
#' reproductive activity, nor will they be recognised as potential mates by 
#' other individuals.
#'@param max_age_reproduce This is the maximum age at which an individual can be
#' reproductively active. No individuals above this age will engage in any
#' reproductive activity, nor will they be recognised as potential mates by
#' other individuals.
#'@param min_age_feed This is the minimum age at which an individual can eat. No
#' individuals below this age will be able to consume food on the landscape.
#'@param max_age_feed This is the maximum age at which an individual can eat. No
#' individuals above this age will be able to consume food on the landscape.
#'@param food_consume This defines how much food an individual will consume from
#' the cell on which it is feeding. Food consumption can take on any positive
#' real value, and an individual will consume up to this amount if possible (if 
#' not, they will consume however much food is left within their landscape 
#' cell).
#'@param pesticide_consume This defines how much pesticide an individual will
#' consume from the cell on which it resides. Pesticide consumption can take on 
#' any positive real value, and an individual will consume up to this amount if
#' possible (if not, they will consume however much pesticide has been placed on
#' the landscape cell).
#'@param rand_age This argument determines whether individuals in the simulation
#' will be initialised with a random age selected uniformly from zero to 
#' max_age. If FALSE, then all individuals will be initialised at age zero.
#'@param move_distance This is the maximum number of cells that an individual 
#' can move, in any direction, on the landscape during one bout of movement.
#'@param food_needed_surv This is the amount of food that an individual needs to
#' consume to survive. If the individual has not consumed this amount of food 
#' before the age of age_food_threshold, then they will die in the time step.
#'@param pesticide_tolerated_surv This is the amount of pesticide that an 
#' individual can tolerate and still survive. If the individual has consumed 
#' more than this amount of pesticide on or after the age of 
#' age_pesticide_threshold, then they will die in the time step.
#'@param food_needed_repr This is the amount of food that an individual needs to
#' produce one offspring. The total number of offspring that an individual 
#' produces in a time step is the floor value of their food consumption divided 
#' by this value.
#'@param pesticide_tolerated_repr This is the amount of pesticide tolerated 
#' below which an individual can reproduce. Note that individuals above the 
#' threshold can still mate and sire offspring,
#'@param reproduction_type This determines how individuals reproduce; the two 
#' options are "lambda" and "food_based." If "lambda," then the number of 
#' offspring an individual produces is sampled from a Poisson distribution with 
#' a fixed rate parameter lambda_value (potentially adjusted by other factors in
#' the simulation). If "food_based," then the number of offspring produced is 
#' based on the amount of food consumed by the individual.
#'@param mating_distance This is the distance in cells (any direction) away from
#' a focal individual from which they can successfully find and identify a mate 
#' (e.g., if 0, then only individuals on the same cell are potential mates).
#'@param lambda_value This is the rate parameter for the Poisson sampling of 
#' offspring number; it only applies when reproduction_type is set to "lambda."
#'@param movement_bouts This is the number of times an individual can move in a 
#' single time step (i.e., the number of cells that it can potentially visit). 
#' Each time an individual visits a new cell, it can potentially feed or consume 
#' pesticide.
#'@param selfing This determines whether or not self-fertilisation is allowed 
#' when repro is set to "sexual."
#'@param feed_while_moving If TRUE, then individuals will feed in each movement 
#' bout when they arrive to a new landscape cell.
#'@param pesticide_while_moving If TRUE, then individuals will consume pesticide
#' in each movement bout when they arrive to a new landscape cell.
#'@param mortality_type This determines how mortality is enacted in the 
#' simulation. Currently there is only one mortality type possible; mortality 
#' occurs if individuals exceed their maximum age, do not consume enough food, 
#' or consume too much pesticide.
#'@param age_food_threshold This is the age at which mortality associated with 
#' feeding is enacted, so an individual younger than this age will not die if 
#' they have not yet consumed sufficient food to satisfy food_needed_surv.
#'@param age_pesticide_threshold This is the age at which mortality associated 
#' with pesticide consumption is enacted, so an individual younger than this age
#' will not die even if they have exceeded their pesticide threshold.
#'@param farms This is the number of farms to be placed on the landscape. Farms 
#' are placed in blocks of roughly equal sizes using a shortest splitline 
#' algorithm. Farms operate independently in terms of what crops they grow and 
#' pesticides they apply.
#'@param time_steps This is the number of time steps that a simulation will run.
#' Simulations will be terminated before this number if extinction occurs.
#'@param mutation_pr This is the probability of mutation occurring at any locus 
#' of a newly produced offspring.
#'@param crossover_pr This is the probability of crossover between two 
#' homologous loci. This only applies for diploid genomes.
#'@param mutation_type This determines how mutation is modelled. If 0, then a 
#' completely new allele value is drawn from a normal distribution with a mean 
#' of mutation_direction and a standard deviation of 1 (or 1 / sqrt(2) for 
#' diploids, so that the expected standard devation of the sum of both allele 
#' values is 1). If 1, then a new value is drawn from a normal distribution with
#' mean mutation_direction and standard deviation of 1, and this new value is 
#' then added to the existing allele value.
#'@param net_mu_layers This is the proportion of the genome that can evolve. If 
#' 0, then only loci values (green circles in Figure 1) can mutate. If 1, then 
#' loci and the first column of arrows (green circles to first column of blue 
#' squares in Figure 1) can mutate. If 2, then the first two columns of arrows 
#' in Figure 1 can mutate, and so forth. Fewer mutation layers will constrain 
#' the covariance among traits, while more mutation layers will allow the 
#' covariance structure to evolve more readily.
#'@param net_mu_dir  The direction along the network in which net_mu_layers 
#' applies (not loci, green circles in Figure 1, can always mutate). If 1, then 
#' net_mu_layers applies in the direction from loci to traits. If 0, then the 
#' direction applies from traits to loci (i.e., net_mu_dir = 0 and 
#' net_mu_layers = 1 would mean that only the arrow values between the last 
#' hidden layer and traits in Figure 1 could mutate).
#'@param mutation_direction This allows mutations to be biased in one direction.
#' A default value of 0 makes positive or negative allele values equally likely.
#'@param crop_init Initial crop type for each farm. This can be set in one of 
#' two ways. First, the default value "random" will randomly assign each farm to
#' an initial crop to produce. Second, a vector can be used to specify the crop
#' initialised on each farm. The vector must be the same length as the number of
#' farms, and the value of each element 'i' of the vector defines which crop is
#' initialised for each farm i. Hence, a crop_init vector must have as many
#' elements as there are farms, and vector elements must include natural numbers
#' from 1 to the total number of crops.
#'@param crop_rotation_type This determines how crop types are rotated across 
#' the landscape. This can be set in one of two ways. First, a natural number
#' can specify a rotation type: (1) crops will never rotate, (2) a new crop type
#' will be randomly chosen every crop_rotation_time time steps for each farm, 
#' or (3) farms will cycle through crop types in order, with a change from one
#' crop type to another every crop_rotation_time time step. Second, a square 
#' matrix can specify the probability of transition from a focal crop type 
#' (rows) to the next crop type (columns). Matrix rows must therefore sum to 1. 
#' For example, an identity matrix (1s in the diagonal and 0s in the 
#' off-diagonal) would specify crops that never rotate (i.e., crop i always 
#' rotates to itself).
#'@param crop_rotation_time This determines how many time steps a crop is left 
#' before being refreshed and potentially changed. Note that even if the crop 
#' type does not change, this value still has the effect of determining how 
#' often crops are replenished (if some have been eaten since the last time they
#' were replenished).
#'@param pesticide_init Initial pesticide type for each farm. This can be set in
#' one of two ways. First, the default value "random" will randomly assign each 
#' farm to an initial pesticide to apply. Second, a vector can be used to 
#' specify the pesticide initialised on each farm. The vector must be the same 
#' length as the number of farms, and the value of each element 'i' of the 
#' vector defines which pesticide is initialised for each farm i. Hence, a 
#' pesticide_init vector must have as many elements as there are farms, and 
#' vector elements must include natural numbers from 1 to the total number of 
#' pesticides.
#'@param pesticide_rotation_type This determines how pesticide types are rotated
#' across the landscape. This can be set in one of two ways. First, a natural 
#' number can specify a rotation type: (1) pesticides will never rotate, (2) a 
#' new pesticide type will be randomly chosen every pesticide_rotation_time time
#' steps for each farm, or (3) farms will cycle through pesticide types in 
#' order, with a change from one pesticide type to another every 
#' pesticide_rotation_time time step. Second, a square matrix can specify the 
#' probability of transition from a focal pesticide type (rows) to the next 
#' pesticide type (columns). Matrix rows must therefore sum to 1. For example, 
#' an identity matrix (1s in the diagonal and 0s in the off-diagonal) would 
#' specify pesticides that never rotate (i.e., pesticide i always rotates to 
#' itself).
#'@param pesticide_rotation_time This determines how many time steps a pesticide
#' is left before being replenished and potentially changed. Note that unlike 
#' crops, pesticide levels do not decrease on the landscape over time (e.g., 
#' with consumption).
#'@param crop_per_cell This determines the expected amount of crop that is 
#' placed on a single landscape cell. The more crop on a cell, the more that can
#' be potentially consumed by individuals.
#'@param pesticide_per_cell This determines how much pesticide is placed on a 
#' single landscape cell. The higher concentration of pesticide per cell, the 
#' more that individuals on the cell will imbibe and potentially be affected by.
#'@param crop_sd This is the standard deviation of crop number placed on 
#' landscape cells. A default value of 0 assumes that all cells have the same 
#' amount of crop.
#'@param pesticide_sd This is the standard deviation of pesticide applied to 
#' each landscape cell. A default value of 0 assumes that each cell has the same
#' concentration of pesticide applied.
#'@param crop_min This is the minimum amount of crop that is possible to have on
#' a single cell (i.e., crop values will never be initialised to be lower than 
#' this value).
#'@param crop_max This is the maximum amount of crop that is possible to have on
#' a single cell (i.e., crop values will never be initialised to be higher than 
#' this value).
#'@param pesticide_min This is the minimum concentration of pesticide that is 
#' possible to have on a single cell (i.e., pesticide values will never be 
#' initialised to be lower than this value).
#'@param pesticide_max This is the maximum concentration of pesticide that is 
#' possible to have on a single cell (i.e., pesticide values will never be 
#' initialised to be higher than this value).
#'@param crop_number This is the number of unique crops that can exist on the 
#' landscape during the course of a simulation. The maximum number of possible 
#' crops is 10.
#'@param pesticide_number This is the number of unique pesticides that can exist
#' on the landscape during the course of a simulation. The maximum number of 
#' possible pesticides is 10.
#'@param print_inds If TRUE, a CSV file will print in the working directory with
#' every individual and all of their characteristics (i.e., locations, traits, 
#' genomes) in every time step. By default, this is set to FALSE and should only
#' be set to TRUE with extreme caution, as large populations persisting over 
#' long periods of time can produce extremely large CSV files.
#'@param print_gens If TRUE, the time step and the population size will be 
#' printed to the R console as the simulation is running.
#'@param print_last If TRUE, a CSV file will print in the working directory with
#' every individual and all of their characteristics (i.e., locations, traits, 
#' genomes) in only the last time step. Note that for large populations, the 
#' file size generated can be very large (10s to 100s of GBs).
#'@param K_on_birth This is a carrying capacity applied to new individuals 
#' across the entire landscape. If the total number of offspring in a time step 
#' exceeds this value, then offspring are removed at random until the total 
#' number of new offspring equals K_on_birth. In practice, this can help speed 
#' up simulations by avoiding the unnecessary production of individuals when 
#' most will perish.
#'@param pesticide_start This is the time step at which pesticide begins to be 
#' applied. No pesticide will be applied prior to this start time, so 
#' individuals will not experience any effects of pesticide. This can be useful 
#' as a tool to burn in the population prior to introducing pesticide.
#'@param immigration_rate This is the number of immigrant individuals arriving 
#' in the landscape in each time step. Immigrants are initialised in random 
#' locations with the same network structure (Figure 1) as individuals 
#' initialised at the start of the simulation, and with allele values randomly 
#' drawn from a standard normal distribution.
#'@param get_f_coef This determines whether or not inbreeding coefficients will 
#' be calculated for sexual populations and printed off in CSV files. Because 
#' this can add some computation time, it is best to set to FALSE unless it is 
#' needed.
#'@param get_stats If TRUE, a CSV file will print in the working directory with 
#' summary statistics for each time step. This is set to TRUE by default.
#'@param metabolism This determines the rate at which food consumed in previous 
#' time steps is lost in subsequent time steps, which can be especially relevant
#' if food consumed determines survival or reproductive output. Values of 0 mean
#' that stored gains will always persist throughout an individual’s lifetime, 
#' while very high values will model the gains of one time step being wiped out 
#' in subsequent time steps (if, e.g., the objective is to model individuals 
#' needing to consume food successfully in each time step to survive or 
#' reproduce, as opposed to having a feeding life history stage followed by a 
#' mating and reproduction stage).
#'@param baseline_metabolism This fixes a baseline metabolic rate at which food 
#' consumed in previous time steps is lost in subsequent steps. This fixed value
#' is always added to metabolism for each individual. By default, this value is 
#' 0.
#'@param min_age_metabolism This determines the minimum age at which losses of 
#' food consumed in previous time steps enacted by metabolism and 
#' baseline_metabolism can occur.
#'@param max_age_metabolism This determines the maximum age at which losses of 
#' food consumed in previous time steps enacted by metabolism and 
#' baseline_metabolism can occur.
#'@param terrain Insert a custom terrain of different farms, which takes the
#' form of a matrix that includes a sequence of natural numbers in all matrix
#' elements. For example, if there are 4 farms, then all matrix elements must
#' be 1, 2, 3, or 4. Beyond this requirement, there is no restriction on where 
#' different farms are placed; the do not even need to be contiguous on the 
#' landscape. Note that a custom terrain will override the arguments farms, 
#' xdim, and ydim. For example, if the matrix given to the terrain argument has
#' 10 rows and 10 columns, then the simulation will automatically set xdim and
#' ydim equal to 10 without any warnings. Also note that these terrain values do
#' not necessarily need to be farms. Through the use of a custom landscape and 
#' pesticide rotation option, these cells could represent something like 
#' diversionary feeding sites or even buildings or rivers. See vignettes and 
#' other documentation for details.
#'@param trait_means This provides the mean values of the evolving pest traits
#' in the initialised pest population, which by default are zero. To change 
#' mean trait values in the initialised population, the trait_means argument
#' requires a vector of the same length as the number of evolving pest traits
#' defined within the function (i.e., if there are two evolving traits in the
#' simulation, "T1" and "T2", then trait_means should be a vector of length 2).
#' Note that mean trait values may change as the population evolves, so the
#' values in this vector define only the means of the initial population.
#'@return The output in the R console is a list with two elements; the first 
#'element is a vector of parameter values used by the model, and the second 
#'element is the landscape in the simulation. The most relevant output will be
#'produced as CSV files within the working directory. When get_stats = TRUE, 
#'a file named 'population_data.csv' is produced in the working directory. When
#'print_last = TRUE, a complete array of all individuals and their 
#'characteristics is printed for the last time step in the working directory in
#' a file named 'last_time_step.csv' (for large simulations, this file can be 
#' > 1GB in size). When print_inds = TRUE, a complete array of all individuals 
#' in all time steps is produced in the working directory in a file named
#' 'individuals.csv' (use this option with extreme caution for all but the
#' smallest simulations).
#'@examples
#'gmt       <- matrix(data = 0, nrow = 4, ncol = 4);
#'diag(gmt) <- 1;
#'mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 3, indivs = 100, 
#'                          npsize = 100, max_gen = 2, prnt_out = FALSE);
#'sim       <- run_farm_sim(mine_output = mg, N = 100, xdim = 40, ydim = 40, 
#'                          repro = "asexual", time_steps = 1, 
#'                          print_inds = FALSE, print_gens = FALSE,
#'                          print_last = FALSE, get_stats = FALSE);
#'@useDynLib resevol
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
                         pesticide_while_moving = FALSE,
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
                         crop_init = "random",
                         crop_rotation_type = 2,
                         crop_rotation_time = 1,
                         pesticide_init = "random",
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
                         print_last = FALSE,
                         K_on_birth = 1000000,
                         pesticide_start = 0,
                         immigration_rate = 0,
                         get_f_coef = FALSE,
                         get_stats = TRUE,
                         metabolism = 0,
                         baseline_metabolism = 0,
                         min_age_metabolism  = 1,
                         max_age_metabolism  = 9,
                         terrain             = NA,
                         trait_means         = NULL){
  
    if(is.na(terrain)[1] == FALSE){
        xdim  <- dim(terrain)[1];
        ydim  <- dim(terrain)[2];
        farms <- max(terrain);
    }
    
    land <- make_landscape(terrain = terrain, rows = ydim, cols = xdim, 
                           depth = 21, farms = farms);

    move_distance_T             <- check_is_trait(move_distance);
    food_needed_surv_T          <- check_is_trait(food_needed_surv);
    pesticide_tolerated_surv_T  <- check_is_trait(pesticide_tolerated_surv);
    food_needed_repr_T          <- check_is_trait(food_needed_repr);
    pesticide_tolerated_repr_T  <- check_is_trait(pesticide_tolerated_repr);
    mating_distance_T           <- check_is_trait(mating_distance);
    lambda_value_T              <- check_is_trait(lambda_value);
    movement_bouts_T            <- check_is_trait(movement_bouts);
    metabolism_T                <- check_is_trait(metabolism);
    
    # Check to see if these are traits
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
    
    metab_rate <- metabolism;
    if(metabolism_T == TRUE){
        metab_rate <- 0;
    }
    
    if(N < 5){
      stop("Need to start with at least 5 pests");
    }
    if(crop_number > 10){
        stop("Cannot have more than 10 crops");
    }
    if(pesticide_number > 10){
        stop("Cannot have more than 10 pesticides");
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
                            pesticide_while_moving   = pesticide_while_moving,
                            mortality_type           = mortality_type,
                            age_food_threshold       = age_food_threshold,
                            age_pesticide_threshold  = age_pesticide_threshold,
                            metabolism               = metab_rate,
                            baseline_metabolism      = baseline_metabolism,
                            min_age_metabolism       = min_age_metabolism,
                            max_age_metabolism       = max_age_metabolism,
                            trait_means              = trait_means);
    
    sim_results <- sim_crops(pests                    = pest, 
                             land                     = land,
                             time_steps               = time_steps, 
                             mutation_pr              = mutation_pr,
                             crossover_pr             = crossover_pr, 
                             mutation_type            = mutation_type,
                             net_mu_layers            = net_mu_layers, 
                             net_mu_dir               = net_mu_dir,
                             mutation_direction       = mutation_direction,
                             crop_init                = crop_init,
                             crop_rotation_type       = crop_rotation_type,
                             crop_rotation_time       = crop_rotation_time,
                             pesticide_init           = pesticide_init,
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
                             immigration_rate         = immigration_rate,
                             get_f_coef               = get_f_coef,
                             get_stats                = get_stats,
                             metabolism               = metabolism,
                             farms                    = farms);
  
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
                      crop_init = "random",
                      crop_rotation_type = 2,
                      crop_rotation_time = 1,
                      pesticide_init = "random",
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
                      immigration_rate = 0,
                      get_f_coef = FALSE,
                      get_stats  = TRUE,
                      metabolism = 0,
                      farms = 4
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
  crti <- crop_rotation_time;
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
  fcoe <- as.numeric(get_f_coef);
  sttt <- as.numeric(get_stats);
  
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
              86.0,   # 86) pests column for metabolic rate (food lost)
              87.0,   # 87) pests column for baseline metabolic rate (fixed)
              88.0,   # 88) pests column for minimum age of metabolism effects
              89.0,   # 89) pests column for maximum age of metabolism effects
              90.0,   # 90) pests column for mean of Trait 1
              91.0,   # 91) pests column for mean of Trait 2
              92.0,   # 92) pests column for mean of Trait 3
              93.0,   # 93) pests column for mean of Trait 4
              94.0,   # 94) pests column for mean of Trait 5
              95.0,   # 95) pests column for mean of Trait 6
              96.0,   # 96) pests column for mean of Trait 7
              97.0,   # 97) pests column for mean of Trait 8
              98.0,   # 98) pests column for mean of Trait 9
              99.0,   # 99) pests column for mean of Trait 10
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
              farms,  # 142) Total number of farms
              crti,   # 143) Time steps between crop rotation
              crpc,   # 144) Crop production per landscape cell
              0,      # 145) Type of crop production (UNUSED)
              crmn,   # 146) Minimum crop production per landscape cell
              crmx,   # 147) Maximum crop production per landscape cell
              1,      # 148) Type of biopesticide rotation (no longer used)
              prti,   # 149) Time steps between biopesticide rotation
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
              0,      # 170) Realised number of immigrants
              fcoe,   # 171) Are inbreeding coefficients calculated?
              sttt    # 172) Get a CSV printoff of statistics
              );

  paras <- substitute_traits(paras, move_distance, food_needed_surv,
                             pesticide_tolerated_surv, food_needed_repr,
                             pesticide_tolerated_repr, mating_distance,
                             food_consume, pesticide_consume, lambda_value,
                             movement_bouts, metabolism);
  
  if(is.array(pests) == FALSE){
    stop("ERROR: pests must be a 2D array.");
  }
  if(is.array(land) == FALSE){
    stop("ERROR: land must be a 3D array.");
  }
  
  c_rotate     <- crop_transitions(crop_rotation_type, crpN);
  p_rotate     <- pesticide_transitions(pesticide_rotation_type, pesN); 
  c_init       <- initialise_crops(crop_init, crpN, farms);
  p_init       <- initialise_pesticide(pesticide_init, pesN, farms); 

  SIM_RESULTS  <- run_farming_sim(pests, land, paras, c_rotate, p_rotate,
                                  c_init, p_init);

  return(SIM_RESULTS);
}

run_farming_sim <- function(IND, LAND, PARAS, CROT, PROT, CINIT, PINIT){
  .Call("sim_farming", IND, LAND, PARAS, CROT, PROT, CINIT, PINIT);
}

substitute_traits <- function(paras, move_distance, food_needed_surv,
                              pesticide_tolerated_surv, food_needed_repr,
                              pesticide_tolerated_repr, mating_distance,
                              food_consume, pesticide_consume, lambda_value,
                              movement_bouts, metabolism){
  
    Tst <- paras[110] - 1;
  
    move_distance_T             <- check_is_trait(move_distance);
    food_needed_surv_T          <- check_is_trait(food_needed_surv);
    pesticide_tolerated_surv_T  <- check_is_trait(pesticide_tolerated_surv);
    food_needed_repr_T          <- check_is_trait(food_needed_repr);
    pesticide_tolerated_repr_T  <- check_is_trait(pesticide_tolerated_repr);
    mating_distance_T           <- check_is_trait(mating_distance);
    lambda_value_T              <- check_is_trait(lambda_value);
    movement_bouts_T            <- check_is_trait(movement_bouts);
    metabolism_T                <- check_is_trait(metabolism);
    
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
    
    if(metabolism_T == TRUE){
      metabolism_N     <- get_trait_number(metabolism) + Tst;
      paras[87]        <- metabolism_N;
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
