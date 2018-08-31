# ==============================================================================
# This is just a toy version of the model, for the presentation
# ==============================================================================
# Note that I'm adding 'toy' at the start of each function; this is just in case
# we actually want to keep some of these functions in the eventual R package, 
# but also don't want to worry about not being able to name functions like
# "initialise_land" the same here as elsewhere in the eventual bigger model.
# ==============================================================================
toy_simulate_resistance <- function(generations = 20,       # Generations to sim
                                    xdim = 2,               # Land dimension 1
                                    ydim = 2,               # Land dimension 2
                                    pathogens = 1,          # Pathogen strains
                                    crops = 1,              # Crop species
                                    path_alleles = 3,       # Pathogen alleles
                                    crop_alleles = 3,       # Crop alleles
                                    pest_init = 2000,       # Initial pests 
                                    crop_rotate = "static", # Crops rotated
                                    path_rotate = "static", # Pathogens rotated
                                    pest_move_pr = 0.1,     # Pest movement
                                    pest_move_dist = 1,     # Pest move distance
                                    fecundity = 8,          # Offspring per fem
                                    cell_K = 2000           # K per cell
                                    ){
    
    if(pest_move_dist > xdim & pest_move_dist > ydim){
        pest_move_dist <- max(c(xdim, ydim)); # Avoids error
    }
    # Start initialising the landscape and pests
    LAND <- toy_initialise_land(xdim  = xdim, ydim = ydim, 
                                pathogens = pathogens, crops = crops);
    PEST <- toy_initialise_pest(LAND, N = pest_init, p_al = path_alleles, 
                                c_al = crop_alleles);
    # Start the generations
    PEST_DATA   <- NULL;
    LAND_DATA   <- NULL;
    gen         <- 1;
    while(gen < generations){
        LAND <- toy_set_crops(LAND, crops, crop_rotate);                        
        LAND <- toy_set_paths(LAND, pathogens, path_rotate);                    
        PEST <- toy_move_pest(PEST, LAND, pest_move_pr, pest_move_dist);
        # ------------- Collecte some data
        PEST_DATA[[gen]] <- PEST;
        LAND_DATA[[gen]] <- LAND;
        # ------------- Back to the biology
        PEST <- toy_feed_pest(PEST, LAND);                                      
        if(toy_check_extinction(PEST, gen) == TRUE){ # Hate these if breaks here
            break;
        }
        PEST <- toy_kill_pest(PEST, LAND);
        if(toy_check_extinction(PEST, gen) == TRUE){
            break;
        }
        PEST <- toy_reproduce_pest(PEST, LAND, path_alleles, crop_alleles, 
                                   fecundity, cell_K);
        if(toy_check_extinction(PEST, gen) == TRUE){
            break;
        }
        gen <- gen + 1;
    }
    return(list(PEST_DATA = PEST_DATA, LAND_DATA = LAND_DATA));
}

results_to_json <- function(pest, land, printit = TRUE, filename = "sim.json"){
    if("package:jsonlite" %in% search() == FALSE){
        stop("Error: Need to load the R package 'jsonlite'")
    }
    gens   <- length(pest) - 2;
    if(gens > 0){
        pest <- pest[[gens]];
        land <- land[[gens]];
    }else{
        stop("Not enough generations to continue");
    }
    inds   <- dim(pest)[1];
    cells  <- dim(land)[1] * dim(land)[2];
    s_size <- cells * 100;
    if(inds > s_size){
        keep <- sample(x = 1:inds, size = s_size, replace = FALSE);
        pest <- pest[keep,];
        inds <- s_size;
    }
    p_geno <- rep(x = 0, times = inds);
    c_geno <- rep(x = 0, times = inds);
    path   <- rep(x = 0, times = inds);
    crop   <- rep(x = 0, times = inds);
    r_path <- rep(x = 0, times = inds);
    r_crop <- rep(x = 0, times = inds);
    for(i in 1:inds){ # Doing this a bit lazily, without refactoring the rest
        p_geno[i] <- as.numeric( paste(pest[i,5], pest[i,6], sep = ""));
        c_geno[i] <- as.numeric( paste(pest[i,7], pest[i,8], sep = ""));
        xloc      <- pest[i, 3];
        yloc      <- pest[i, 4];
        path[i]   <- land[xloc, yloc, 2];
        crop[i]   <- land[xloc, yloc, 3];
        if(pest[i,5] == path[i] | pest[i,6] == path[i]){
            r_path[i] <- 1;
        }
        if(pest[i,7] == crop[i] | pest[i,8] == crop[i]){
            r_crop[i] <- 1;
        }
    }
    data      <- matrix(data = 0, nrow = inds, ncol = 10);
    data[,1]  <- pest[,1];
    data[,2]  <- pest[,2];
    data[,3]  <- pest[,3];
    data[,4]  <- pest[,4];
    data[,5]  <- path;
    data[,6]  <- crop;
    data[,7]  <- p_geno;
    data[,8]  <- c_geno;
    data[,9]  <- r_path;
    data[,10] <- r_crop;
    xdim <- dim(land)[1];
    ydim <- dim(land)[2];
    rows <- xdim * ydim;
    locs <- as.matrix(expand.grid(1:xdim, 1:ydim));
    mat  <- cbind(locs, matrix(data = 0, ncol = 7, nrow = dim(locs)[1]));
    path <- rep(x = 0, times = dim(mat)[1]);
    crop <- rep(x = 0, times = dim(mat)[1]);
    resr <- rep(x = 0, times = dim(mat)[1]);
    eatr <- rep(x = 0, times = dim(mat)[1]);
    for(i in 1:dim(mat)[1]){
        yloc      <- mat[i, 1];
        xloc      <- mat[i, 2];
        path[i]   <- land[xloc, yloc, 2];
        crop[i]   <- land[xloc, yloc, 3];
        inds_on   <- pest[pest[,3] == xloc & pest[,4] == yloc,];
        if(length(inds_on) > dim(pest)[2]){
            pop_size  <- dim(inds_on)[1];
            genos_pth <- as.numeric(paste(inds_on[,5], inds_on[,6], sep = ""));
            getyp_pth <- length(unique(genos_pth));
            raw_res   <- sum(inds_on[,5] == path[i] | inds_on[,6] == path[i]);
            genos_eat <- as.numeric(paste(inds_on[,7], inds_on[,8], sep = ""));
            getyp_eat <- length(unique(genos_eat));
            raw_eat   <- sum(inds_on[,7] == crop[i] | inds_on[,8] == crop[i]);
            pct_res   <- 100 * raw_res / pop_size;
            pct_eat   <- 100 * raw_eat / pop_size;
            mat[i, 3] <- crop[i];
            mat[i, 4] <- path[i];
            mat[i, 5] <- pop_size;
            mat[i, 6] <- getyp_pth;
            mat[i, 7] <- getyp_eat;
            mat[i, 8] <- pct_res;
            mat[i, 9] <- pct_eat;
            resr[i]   <- raw_res;
            eatr[i]   <- raw_eat;
        }        
    }
    population  <- dim(pest)[1];
    p_genos     <- as.numeric(paste(pest[,5], pest[,6], sep = ""));
    p_genotypes <- length(unique(p_genos));
    c_genos     <- as.numeric(paste(pest[,7], pest[,8], sep = ""));
    c_genotypes <- length(unique(c_genos));
    pct_resist  <- 100 * sum(resr) / population;
    pct_eaters  <- 100 * sum(eatr) / population;
    landscape   <- c(population, p_genotypes, c_genotypes, pct_resist, 
                     pct_eaters);
    colnames(data) <- c("ID", "sex", "xloc", "yloc", "path", "crop", 
                        "p_geno", "c_geno", "resist_path", "eat_crop");
    names(landscape) <- c("pop_size", "resist_genotypes", "crop_genotypes", 
                          "percentage_resistant", "percentage_crop_eaters");
    colnames(mat)    <- c("xloc", "yloc", "crop", "pathogen", "pop_size", 
                          "genotypes_resist", "genotypes_crop", 
                          "percentage_resistant", "percentage_crop_eaters");
    modsim <- list( landscape = names(landscape),
                    land_vals = unname(landscape),
                    cells     = colnames(mat),
                    cell_vals = unname(apply(mat, 1, 
                                             function(x) as.data.frame(t(x)))),
                    traits = colnames(data), 
                    values    = unname(apply(data, 1, 
                                             function(x) as.data.frame(t(x))))
    );
    sim_json <- toJSON(list(landscape = names(modsim), cells = names(modsim), 
                            values = modsim), pretty = TRUE);
    if(printit == TRUE){
        write(sim_json, filename);
    }
    return(sim_json);
}

summarise_pest_data <- function(PEST_DATA){
    # Density estimates
    den_list  <- unlist(lapply(PEST_DATA, dim));
    den_vect  <- den_list[c(TRUE, FALSE)];
    gens      <- length(PEST_DATA);
    p_alleles <- max(c(PEST_DATA[[1]][,5], PEST_DATA[[1]][,6]));
    c_alleles <- max(c(PEST_DATA[[1]][,7], PEST_DATA[[1]][,8])); 
    p_tabl    <- matrix(data = 0, nrow = gens, ncol = p_alleles);
    c_tabl    <- matrix(data = 0, nrow = gens, ncol = c_alleles);
    # Allele frequencies
    for(gen in 1:length(PEST_DATA)){
        total_p_alleles <- length(PEST_DATA[[gen]][,5:6]);
        total_c_alleles <- length(PEST_DATA[[gen]][,7:8]);
        for(allele in 1:p_alleles){
            allele_count         <- sum(PEST_DATA[[gen]][,5:6] == allele);
            p_tabl[gen, allele]  <- allele_count / total_p_alleles
        }
        for(allele in 1:c_alleles){
            allele_count         <- sum(PEST_DATA[[gen]][,7:8] == allele);
            c_tabl[gen, allele]  <- allele_count / total_c_alleles
        }
    }
    return(list(densities = den_vect, pathogen_fr = p_tabl, crop_fr = c_tabl));
}

toy_check_extinction <- function(PEST, gen){
    if(is.vector(PEST) == TRUE){
        return(TRUE);
    }
    if(dim(PEST)[1] < 4){
        return(TRUE);
    }
    if(sum(PEST[,2] == 0) < 1 | sum(PEST[,2] == 1) < 1){
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
toy_set_crops <- function(LAND, crops = 1, type = "static"){
    if(crops == 1 | type == "static"){
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
toy_set_paths <- function(LAND, paths = 1, type = "static"){
    if(paths == 1 | type == "static"){
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
        LAND[,,2] <- new_path;
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
toy_reproduce_pest <- function(PEST, LAND, pa, cr, births = 2, K = 100){
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
    offspring <- build_new_pest(offspring, total_offspring, pa, cr);
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
build_new_pest <- function(offspring, total_offspring, pa, cr, mutation = 0.01){
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
    new_PEST[mu5, 5] <- sample(x = 1:cr, size = length(mu5), replace = TRUE);
    new_PEST[mu6, 6] <- sample(x = 1:cr, size = length(mu6), replace = TRUE);
    new_PEST[mu7, 7] <- sample(x = 1:pa, size = length(mu7), replace = TRUE);
    new_PEST[mu8, 8] <- sample(x = 1:pa, size = length(mu8), replace = TRUE);
    return(new_PEST);
}


