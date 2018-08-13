# ==============================================================================
# This is just a toy version of the model, for the presentation
# ==============================================================================
# Note that I'm adding 'toy' at the start of each function; this is just in case
# we actually want to keep some of these functions in the eventual R package, 
# but also don't want to worry about not being able to name functions like
# "initialise_land" the same here as elsewhere in the eventual bigger model.
# ==============================================================================

toy_simulate_resistance <- function(generations = 10, xdim = 2, ydim = 2, 
                                    pathogens = 1, crops = 1, pest_init = 100, 
                                    crop_rotate = "random", 
                                    path_rotate = "random", pest_move_pr = 0.1,
                                    pest_move_dist = 1, fecundity = 2, 
                                    cell_K = 100){
    
    if(pest_move_dist > xdim & pest_move_dist > ydim){
        pest_move_dist <- max(c(xdim, ydim)); # Avoids error
    }
    # Start initialising the landscape and pests
    LAND <- toy_initialise_land(xdim  = xdim, ydim = ydim, 
                                pathogens = pathogens, crops = crops);
    PEST <- toy_initialise_pest(LAND, N = pest_init, p_al = pathogens, 
                                c_al = crops);
    # Start the generations
    PEST_DATA   <- NULL;
    gen         <- 1;
    exti        <- FALSE;
    while(gen < generations){
        LAND <- toy_set_crops(LAND, crops, crop_rotate);
        LAND <- toy_set_paths(LAND, pathogens, path_rotate);
        PEST <- toy_move_pest(PEST, LAND, pest_move_pr, pest_move_dist);
        PEST <- toy_feed_pest(PEST, LAND);
        if(toy_check_extinction(PEST, gen) == TRUE){ # Hate these if breaks here
            break;
        }
        PEST <- toy_kill_pest(PEST, LAND);
        if(toy_check_extinction(PEST, gen) == TRUE){
            break;
        }
        PEST <- toy_reproduce_pest(PEST, LAND, fecundity, cell_K);
        if(toy_check_extinction(PEST, gen) == TRUE){
            break;
        }
        PEST_DATA[[gen]] <- PEST;
        gen <- gen + 1;
    }
    return(PEST_DATA);
}

toy_check_extinction <- function(PEST, gen){
    if(is.vector(PEST) == TRUE){
        print(paste("Extinction occurred in generation", gen));
        return(TRUE);
    }
    if(dim(PEST)[1] < 4){
        print(paste("Extinction occurred in generation", gen));
        return(TRUE);
    }
    if(sum(PEST[,2] == 0) < 1 | sum(PEST[,2] == 1) < 1){
        print(paste("Extinction occurred in generation", gen));
        return(TRUE);
    }
    return(FALSE);
}

# Initialise the landscape
toy_initialise_land <- function(xdim = 2, ydim = 2, pathogens = 1, crops = 1){
    LAND      <- array(data = 0, dim = c(xdim, ydim, 3));
    p_values  <- sample(x = 1:pathogens, size = xdim * ydim, replace = TRUE);
    p_layer   <- matrix(data = p_values, ncol = xdim, nrow = ydim);
    c_values  <- sample(x = 1:crops, size = xdim * ydim, replace = TRUE);
    c_layer   <- matrix(data = c_values, ncol = xdim, nrow = ydim);
    LAND[,,2] <- p_layer; # Just occurred to me that we might want a layer for
    LAND[,,3] <- c_layer; # carrying capacity of the cell in LAND later
    return(LAND);
}

# Initialise some *very* simple pests. Each allele is just going to map one to
# one for whether a crop can be attacked or a pathogen resisted. I am not even
# going to add traits affecting dispersal in the toy version -- this can be
# uniform for all individuals
toy_initialise_pest <- function(LAND, N = 10, p_al = 1, c_al = 1){
    xdim     <- dim(LAND)[1]; # This is needed to put the pests on some
    ydim     <- dim(LAND)[2]; # landscape cell
    PEST     <- matrix(data = 0, nrow = N, ncol = 8);
    PEST[,1] <- 1:N;                                           # ID
    PEST[,2] <- sample(x = c(0, 1), size = N, replace = TRUE); # Sex
    PEST[,3] <- sample(x = 1:xdim,  size = N, replace = TRUE); # x-location
    PEST[,4] <- sample(x = 1:ydim,  size = N, replace = TRUE); # y-location
    PEST[,5] <- sample(x = 1:p_al,  size = N, replace = TRUE); # p allele 1
    PEST[,6] <- sample(x = 1:p_al,  size = N, replace = TRUE); # p allele 2
    PEST[,7] <- sample(x = 1:c_al,  size = N, replace = TRUE); # c allele 1
    PEST[,8] <- sample(x = 1:c_al,  size = N, replace = TRUE); # c allele 2
    return(PEST);
}

# Just going to make this a simple function at first, randomly changing crops
toy_set_crops <- function(LAND, crops = 1, type = "rotate"){
    if(crops == 1 | crops == "static"){
        return(LAND);
    }
    if(type == "rotate"){
        old_crop                   <- LAND[,,3];
        new_crop                   <- old_crop + 1;
        new_crop[new_crop > crops] <- 1;
        LAND[,,3]                  <- new_crop;
    }else{ # Else it just randomises -- can think about fancier ways later
        xdim      <- dim(LAND)[1];
        ydim      <- dim(LAND)[2];
        new_cval  <- sample(x = 1:crops, size = xdim * ydim, replace = TRUE);
        new_crop  <- matrix(data = new_cval, nrow = xdim, ncol = ydim);
        LAND[,,3] <- new_crop;
    }
    return(LAND);
}

# Ditto here -- just a simple function changing pathogens
toy_set_paths <- function(LAND, paths = 1, type = "rotate"){
    if(paths == 1){
        return(LAND);
    }
    if(type == "rotate"){
        old_path                   <- LAND[,,3];
        new_path                   <- old_path + 1;
        new_path[new_path > paths] <- 1;
        LAND[,,2]                  <- new_path;
    }else{ # Else it just randomises -- can think about fancier ways later
        xdim      <- dim(LAND)[1];
        ydim      <- dim(LAND)[2];
        new_pval  <- sample(x = 1:paths, size = xdim * ydim, replace = TRUE);
        new_path  <- matrix(data = new_pval, nrow = xdim, ncol = ydim);
        LAND[,,3] <- new_path;
    }
    return(LAND);
}

# Just a function to move pests at a given value, randomly on the landscape. The
# `prob` is just probability of moving in a time step, while `dist` is just the
# maximum distance moved in cells, in any direction.
toy_move_pest <- function(PEST, LAND, prob = 0.1, dist = 1){
    xdim                     <- dim(LAND)[1];
    ydim                     <- dim(LAND)[2];
    pests                    <- dim(PEST)[1];
    to_move                  <- rbinom(n = pests, size = 1, pr = prob);
    move_x                   <- sample(x = -dist:dist, size = pests, 
                                       replace = TRUE);
    move_y                   <- sample(x = -dist:dist, size = pests, 
                                       replace = TRUE);
    PEST[to_move == 1, 3]    <- PEST[to_move == 1, 3] + move_x[to_move == 1];
    PEST[to_move == 1, 4]    <- PEST[to_move == 1, 4] + move_y[to_move == 1];
    # The ifs below make a torus landscape so that pests don't move off of it
    # In C, this will be done with a loop that looks cleaner; the below is just
    # avoiding using a loop in R, though it would probably be fine.
    if(length(PEST[PEST[,3] < 1, 3]) > 0){ 
        PEST[PEST[,3] < 1, 3] <- PEST[PEST[,3] < 1, 3] + xdim;
    }
    if(length(PEST[PEST[,3] > xdim, 3]) > 0){
        PEST[PEST[,3] > xdim, 3] <- PEST[PEST[,3] > xdim, 3] - xdim;
    }
    if(length(PEST[PEST[,4] < 1, 4]) > 0){
        PEST[PEST[,4] < 1, 4] <- PEST[PEST[,4] < 1, 4] + ydim;
    }
    if(length(PEST[PEST[,4] > ydim, 4]) > 0){
        PEST[PEST[,4] > ydim, 4] <- PEST[PEST[,4] > ydim, 4] - ydim;
    }
    return(PEST);
}

# Need to now feed the pests and remove those that don't feed (crop unavailable)
toy_feed_pest <- function(PEST, LAND){
    # I'm just going to use a loop here, else matching to LAND cells is rough
    pests <- dim(PEST)[1];
    eaten <- rep(x = 0, times = pests); 
    for(i in 1:pests){
        x_loc <- PEST[i, 3];
        y_loc <- PEST[i, 4];
        food  <- LAND[x_loc, y_loc, 3];
        if(PEST[i, 7] == food | PEST[i, 8] == food){
            eaten[i] <- 1;
        }
    }
    PEST <- PEST[eaten == 1,]; # You don't eat, you don't live
    return(PEST);
}

# Then need to have pests resist pathogens, kill those that cannot
toy_kill_pest <- function(PEST, LAND){
    # I'm just going to use a loop here, else matching to LAND cells is rough
    pests    <- dim(PEST)[1];
    survived <- rep(x = 0, times = pests);
    for(i in 1:pests){
        x_loc <- PEST[i, 3];
        y_loc <- PEST[i, 4];
        patho <- LAND[x_loc, y_loc, 2];
        if(PEST[i, 5] == patho | PEST[i, 6] == patho){
            survived[i] <- 1;
        }
    }
    PEST <- PEST[survived == 1,]; # You don't eat, you don't live
    return(PEST);
}

# Then, reproduce pests that are left
toy_reproduce_pest <- function(PEST, LAND, births = 2, K = 100){
    x_dim           <- dim(LAND)[1];
    y_dim           <- dim(LAND)[2];
    offspring       <- NULL;
    total_offspring <- 0; # This and the below are just to avoid an rbind(),
    cell            <- 1; # which is a massive memory sink
    lst_ID          <- max(PEST[,1]);
    for(xloc in 1:x_dim){ # Double loop is just much cleaner here
        for(yloc in 1:y_dim){
            locals     <- which(PEST[,3] == xloc & PEST[,4] == yloc);
            local_offs <- NULL;
            if( length(locals) > 1 ){
                local_offs <- toy_breed_locals(PEST, locals, births, K, lst_ID);
            }
            if(length(local_offs) > 0){
                lst_ID            <- lst_ID + dim(local_offs)[1];
                total_offspring   <- total_offspring + dim(local_offs)[1];
                offspring[[cell]] <- local_offs;
            }else{
                offspring[[cell]] <- NULL;
            }
            cell              <- cell + 1;
        }
    }
    offspring <- build_new_pest(offspring, total_offspring);
    return(offspring);
}

# Need to recombine the genomes correctly in the offspring
toy_breed_locals <- function(PEST, locals, births, K, last_ID){
    loc_PEST <- PEST[locals,]; # Need two of each sex (allee effect)
    if(sum(loc_PEST[,2] == 0) < 2 | sum(loc_PEST[,2] == 1) < 2){
        return(NULL);
    }
    females       <- loc_PEST[loc_PEST[,2] == 0,];
    males         <- loc_PEST[loc_PEST[,2] == 1,];
    new_offs      <- dim(females)[1] * floor(births);  
    offspring     <- matrix(data = 0, nrow = new_offs, ncol = 8);     
    offspring[,1] <- (last_ID + 1):(last_ID + new_offs);
    offspring[,2] <- sample(x = c(0, 1), size = new_offs, replace = TRUE);
    offspring[,3] <- loc_PEST[1, 3];
    offspring[,4] <- loc_PEST[1, 4];
    # Below grabs all of the alleles from females and males
    p_fem_alleles <- c(females[,5], females[,6]);
    p_mal_alleles <- c(males[,5], males[,6]);
    c_fem_alleles <- c(females[,7], females[,8]);
    c_mal_alleles <- c(males[,7], males[,8]);
    # Now add them to the offspring randomly, one from female and one from male
    for(i in 1:new_offs){
        if(runif(n = 1) < 0.5){
            offspring[i, 5] <- sample(x = p_fem_alleles, size = 1);
            offspring[i, 6] <- sample(x = p_mal_alleles, size = 1);
        }else{
            offspring[i, 5] <- sample(x = p_mal_alleles, size = 1);
            offspring[i, 6] <- sample(x = p_fem_alleles, size = 1);            
        }
        if(runif(n = 1) < 0.5){
            offspring[i, 7] <- sample(x = c_fem_alleles, size = 1);
            offspring[i, 8] <- sample(x = c_mal_alleles, size = 1);
        }else{
            offspring[i, 7] <- sample(x = c_mal_alleles, size = 1);
            offspring[i, 8] <- sample(x = c_fem_alleles, size = 1);            
        }
    }
    if(dim(offspring)[1] > K){
        offspring <- offspring[1:K,];
    }
    return(offspring);
}

# Merge the different layers into a new pest array
build_new_pest <- function(offspring, total_offspring, mutation = 0.01){
    new_PEST   <- matrix(data = 0, nrow = total_offspring, ncol = 8);
    start_row  <- 1; # Again, avoids an rbind memory issue
    for(cell in 1:length(offspring)){
        if(length(offspring[[cell]]) > 0){
            cell_offs       <- dim(offspring[[cell]])[1];
            rows            <- start_row:(start_row + cell_offs - 1);
            new_PEST[rows,] <- offspring[[cell]];
            start_row       <- start_row + cell_offs;
        }
    }
    mu5 <- which(rbinom(n = total_offspring, size = 1, pr = mutation) == 1);
    mu6 <- which(rbinom(n = total_offspring, size = 1, pr = mutation) == 1);
    mu7 <- which(rbinom(n = total_offspring, size = 1, pr = mutation) == 1);
    mu8 <- which(rbinom(n = total_offspring, size = 1, pr = mutation) == 1);
    new_PEST[mu5, 5] <- sample(x = 1:10, size = length(mu5), replace = TRUE);
    new_PEST[mu6, 6] <- sample(x = 1:10, size = length(mu6), replace = TRUE);
    new_PEST[mu7, 7] <- sample(x = 1:10, size = length(mu7), replace = TRUE);
    new_PEST[mu8, 8] <- sample(x = 1:10, size = length(mu8), replace = TRUE);
    return(new_PEST);
}





