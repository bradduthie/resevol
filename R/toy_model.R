
# sim1 <- replicate_toy_sims(generations = 20, xdim = 15, ydim = 15, 
# pathogens = 3, crops = 3, pest_init = 10000, crop_rotate = "rotate", 
# path_rotate = "rotate", cell_K = 100, block_len = 15, print_it = FALSE, 
# fecundity = 10, pest_move_dist = 1)

# ==============================================================================
# This is just a toy version of the model, for the presentation
# ==============================================================================
# Note that I'm adding 'toy' at the start of each function; this is just in case
# we actually want to keep some of these functions in the eventual R package, 
# but also don't want to worry about not being able to name functions like
# "initialise_land" the same here as elsewhere in the eventual bigger model.
# ==============================================================================
replicate_toy_sims <- function(generations = 20,       # Generations to sim
                               xdim = 100,             # Land dimension 1
                               ydim = 100,             # Land dimension 2
                               pathogens = 3,          # Pathogen strains
                               crops = 3,              # Crop species
                               path_alleles = 3,       # Pathogen alleles
                               crop_alleles = 3,       # Crop alleles
                               pest_init = 50000,      # Initial pests 
                               crop_rotate = "rotate", # Crops rotated
                               path_rotate = "rotate", # Pathogens rotated
                               pest_move_pr = 0.1,     # Pest movement
                               pest_move_dist = 5,     # Pest move distance
                               fecundity = 500,        # Offspring per fem
                               cell_K = 1000,          # K per cell
                               print_gen = TRUE,       # Option print gen
                               pois_move = TRUE,       # Kind of movement
                               land_bloc = TRUE,       # Blocked land  
                               block_len = 1,          # Length block
                               rep_num   = 1,          # How many times
                               crop_freq = 10,         # Rel freq crop change
                               path_freq = 10,         # Rel freq path change
                               print_it  = TRUE,       # Print to file
                               print_file = "toy.csv"  # Print to file
                              ){
  results    <- NULL;
  while(rep_num > 0){
      sim <- toy_simulate_resistance(generations = generations,
                                     xdim = xdim,            
                                     ydim = ydim,             
                                     pathogens = pathogens,          
                                     crops = crops,              
                                     path_alleles = path_alleles,       
                                     crop_alleles = crop_alleles,       
                                     pest_init = pest_init,      
                                     crop_rotate = crop_rotate, 
                                     path_rotate = path_rotate, 
                                     pest_move_pr = pest_move_pr,     
                                     pest_move_dist = pest_move_dist,     
                                     fecundity = fecundity,       
                                     cell_K = cell_K,          
                                     print_gen = print_gen,     
                                     pois_move = pois_move,    
                                     land_bloc = land_bloc,  
                                     block_len = block_len,
                                     crop_freq = crop_freq,
                                     path_freq = path_freq);
      sim_summ <- summarise_gens(sim);
      rows_tot <- 1;
      if(is.matrix(sim_summ) == TRUE){
          rows_tot   <- dim(sim_summ)[1];
          rep_col    <- rep(x = rep_num, times = rows_tot);
          sim_summ   <- cbind(rep_col, sim_summ);
          results    <- rbind(results, sim_summ);
      }else{
          results    <- c(rows_tot, sim_summ);
      }
      rep_num    <- rep_num - 1;
  }
  if(print_it == TRUE){
      write.csv(x = results, file = print_file);
  }
  return(results);
}


toy_collect_file_output <- function(dir, file = FALSE){
    files    <- list.files(path = dir);
    chunks   <- unlist(strsplit(x = files, split = "_"));
    numbers  <- gsub("[^0-9\\]", NA, chunks);
    only_num <- as.numeric(numbers[!is.na(numbers)]);
    last_gen <- max(only_num);
    sim      <- NULL;
    for(i in 2:last_gen){
        LAND1_name <- paste(dir, "/", "LAND_", i, "_1_.csv", sep = "");
        LAND2_name <- paste(dir, "/", "LAND_", i, "_2_.csv", sep = "");
        LAND3_name <- paste(dir, "/", "LAND_", i, "_3_.csv", sep = "");
        LAND1      <- read.csv(file = LAND1_name);
        LAND2      <- read.csv(file = LAND2_name);
        LAND3      <- read.csv(file = LAND3_name);
        LAND1      <- LAND1[,-1];
        LAND2      <- LAND2[,-1];
        LAND3      <- LAND3[,-1];
        LAND_D     <- dim(LAND1);
        LVEC1      <- as.vector(unlist(LAND1));
        LVEC2      <- as.vector(unlist(LAND2));
        LVEC3      <- as.vector(unlist(LAND3));
        LAND       <- array(data = c(LVEC1, LVEC2, LVEC3), dim = c(LAND_D, 3));
        PEST_name  <- paste(dir, "/", "PEST_", i-1, ".csv", sep = "");
        PEST       <- read.csv(file = PEST_name);
        PEST       <- PEST[,-1];
        if(file != FALSE){
          res            <- summarise_sim_gen(pest = PEST, land = LAND);
          land_vec       <- c(i, as.vector(res$landscape));
          write(x = as.vector(land_vec), file = file, append = TRUE);
          gc();
        }else{
            sim$LAND_DATA[[i]] <- LAND;
            sim$PEST_DATA[[i]] <- PEST;
        }
        print(paste("Scanned generation", i, "of", last_gen));
    }
    return(sim);
}

# This is the workhorse function that actually simulates resistance
toy_simulate            <- function(generations = 20,       # Generations to sim
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
                                    pest_move_dist = 5,     # Pest move distance
                                    fecundity = 8,          # Offspring per fem
                                    cell_K = 2000,          # K per cell
                                    print_gen = TRUE,       # Option print gen
                                    pois_move = TRUE,       # Kind of movement
                                    land_bloc = TRUE,       # Blocked land  
                                    block_len = 1,          # Length block
                                    crop_freq = 10,         # Freq crop change
                                    path_freq = 10,         # Freq path change
                                    print_dir = "toy"       # Print directory
                                    ){
    if(pest_move_dist > xdim & pest_move_dist > ydim){
        pest_move_dist <- max(c(xdim, ydim)); # Avoids error
    }
    # Start initialising the landscape and pests
    if(land_bloc == TRUE){
        LAND <- toy_block_land(xdim = xdim, ydim = ydim, 
                               pathogens = pathogens, crops = crops, 
                               block_len = block_len);
    }else{
        LAND <- toy_initialise_land(xdim  = xdim, ydim = ydim, 
                                pathogens = pathogens, crops = crops);
    }
    PEST <- toy_initialise_pest(LAND, N = pest_init, p_al = path_alleles, 
                                c_al = crop_alleles);
    # Start the generations
    gen         <- 1;
    while(gen <= generations){
        if(gen > 1){
            LAND <- NEW_LAND;
            PEST <- NEW_PEST;
            rm(NEW_LAND);
            rm(NEW_PEST);
            gc(); 
        }
        if(gen %% crop_freq == 0){
            LAND <- toy_set_crops(LAND, crops, crop_rotate);
        }
        if(gen %% path_freq == 0){
            LAND <- toy_set_paths(LAND, pathogens, path_rotate);
        }
        if(pois_move == TRUE){
            PEST <- toy_pois_move_pest(PEST, LAND, pest_move_dist);
        }else{
            PEST <- toy_move_pest(PEST, LAND, pest_move_pr, pest_move_dist);
        }
        # ------------- Back to the biology
        PEST <- toy_feed_pest(PEST, LAND);                                      
        if(toy_check_extinction(PEST, gen) == TRUE){ # Hate these if breaks here
            NEW_PEST <- PEST;
            NEW_LAND <- LAND;
            break;
        }
        PEST <- toy_kill_pest(PEST, LAND);
        if(toy_check_extinction(PEST, gen) == TRUE){
            NEW_PEST <- PEST;
            NEW_LAND <- LAND;
            break;
        }
        PEST <- toy_reproduce_pest(PEST, LAND, path_alleles, crop_alleles, 
                                   fecundity, cell_K);
        if(toy_check_extinction(PEST, gen) == TRUE){
            NEW_PEST <- PEST;
            NEW_LAND <- LAND;
            break;
        }
        if(print_gen == TRUE){
            print(paste("Simulating generation ",gen," of ",generations));
        }
        LAND_file_1 <- paste(print_dir, "LAND_", gen, "_1_", ".csv", sep = "");
        LAND_file_2 <- paste(print_dir, "LAND_", gen, "_2_", ".csv", sep = "");
        LAND_file_3 <- paste(print_dir, "LAND_", gen, "_3_", ".csv", sep = "");
        PEST_file   <- paste(print_dir, "PEST_", gen, ".csv", sep = "");
        write.csv(x = LAND[,,1], file = LAND_file_1);
        write.csv(x = LAND[,,2], file = LAND_file_2);
        write.csv(x = LAND[,,3], file = LAND_file_3);
        write.csv(x = PEST, file = PEST_file);
        NEW_LAND <- LAND;
        NEW_PEST <- PEST; 
        rm(LAND);
        rm(PEST);
        gc();
        gen <- gen + 1;
    }
    return(list(PEST = NEW_PEST, LAND = NEW_LAND));
}

# This is the workhorse function that actually simulates resistance
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
                                    pest_move_dist = 5,     # Pest move distance
                                    fecundity = 8,          # Offspring per fem
                                    cell_K = 2000,          # K per cell
                                    print_gen = TRUE,       # Option print gen
                                    pois_move = TRUE,       # Kind of movement
                                    land_bloc = TRUE,       # Blocked land  
                                    block_len = 1,          # Length block
                                    crop_freq = 10,         # Freq crop change
                                    path_freq = 10          # Freq path change
){
    if(pest_move_dist > xdim & pest_move_dist > ydim){
        pest_move_dist <- max(c(xdim, ydim)); # Avoids error
    }
    # Start initialising the landscape and pests
    if(land_bloc == TRUE){
        LAND <- toy_block_land(xdim = xdim, ydim = ydim, 
                               pathogens = pathogens, crops = crops, 
                               block_len = block_len);
    }else{
        LAND <- toy_initialise_land(xdim  = xdim, ydim = ydim, 
                                    pathogens = pathogens, crops = crops);
    }
    PEST <- toy_initialise_pest(LAND, N = pest_init, p_al = path_alleles, 
                                c_al = crop_alleles);
    # Start the generations
    PEST_DATA   <- NULL;
    LAND_DATA   <- NULL;
    gen         <- 1;
    while(gen <= generations){
        if(gen %% crop_freq == 0){
            LAND <- toy_set_crops(LAND, crops, crop_rotate);
        }
        if(gen %% path_freq == 0){
            LAND <- toy_set_paths(LAND, pathogens, path_rotate);
        }
        if(pois_move == TRUE){
            PEST <- toy_pois_move_pest(PEST, LAND, pest_move_dist);
        }else{
            PEST <- toy_move_pest(PEST, LAND, pest_move_pr, pest_move_dist);
        }
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
        if(print_gen == TRUE){
            print(paste("Simulating generation ",gen," of ",generations));
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

# Summarise over generations
summarise_gens <- function(sim, print_gen = TRUE){
    PEST_DATA <- sim$PEST_DATA;
    LAND_DATA <- sim$LAND_DATA;
    gens      <- length(PEST_DATA);
    land_res  <- matrix(data = 0, ncol = 7, nrow = gens);
    for(gen in 1:gens){
      res <- summarise_sim_gen(pest = PEST_DATA[[gen]], 
                               land = LAND_DATA[[gen]]);
      land_vec <- as.vector(res$landscape)
      land_res[gen,] <- c(gen, land_vec);
      if(print_gen == TRUE){
          print(paste("Summarising generation ",gen," of ",gens));
      }
    }
    colnames(land_res) <- c("generation", "pop_size", "resist_genotypes", 
                            "crop_genotypes",  "percentage_resistant", 
                            "percentage_crop_eaters", "percentage_survivors");
    return(land_res);
}

summarise_sim_gen <- function(pest, land){
    inds   <- dim(pest)[1];
    cells  <- dim(land)[1] * dim(land)[2];
    s_size <- cells * 10;
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
        if(i %% 100 == 0){
            print(paste("On ",i," of ",inds));
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
    mat  <- cbind(locs, matrix(data = 0, ncol = 8, nrow = dim(locs)[1]));
    path <- rep(x = 0, times = dim(mat)[1]);
    crop <- rep(x = 0, times = dim(mat)[1]);
    resr <- rep(x = NA, times = dim(mat)[1]);
    eatr <- rep(x = NA, times = dim(mat)[1]);
    srvr <- rep(x = NA, times = dim(mat)[1]);
    for(i in 1:dim(mat)[1]){
        yloc      <- as.numeric(mat[i, 1]);
        xloc      <- as.numeric(mat[i, 2]);
        path[i]   <- land[xloc, yloc, 2];
        crop[i]   <- land[xloc, yloc, 3];
        tot_indon <- sum(pest[,3] == xloc & pest[,4] == yloc);
        if(tot_indon > 1){
            inds_on   <- pest[pest[,3] == xloc & pest[,4] == yloc,];
            pop_size  <- dim(inds_on)[1];
            genos_pth <- as.numeric(paste(inds_on[,5], inds_on[,6], sep = ""));
            getyp_pth <- length(unique(genos_pth));
            raw_res   <- sum(inds_on[,5] == path[i] | inds_on[,6] == path[i]);
            genos_eat <- as.numeric(paste(inds_on[,7], inds_on[,8], sep = ""));
            getyp_eat <- length(unique(genos_eat));
            raw_eat   <- sum(inds_on[,7] == crop[i] | inds_on[,8] == crop[i]);
            raw_sur   <- sum(  (inds_on[,5] == path[i] | inds_on[,6] == path[i])
                             & (inds_on[,7] == crop[i] | inds_on[,8] == crop[i])
                            );
            pct_res   <- 100 * raw_res / pop_size;
            pct_eat   <- 100 * raw_eat / pop_size;
            pct_sur   <- 100 * raw_sur / pop_size;
            mat[i, 3] <- crop[i];
            mat[i, 4] <- path[i];
            mat[i, 5] <- pop_size;
            mat[i, 6] <- getyp_pth;
            mat[i, 7] <- getyp_eat;
            mat[i, 8] <- pct_res;
            mat[i, 9] <- pct_eat;
            mat[i,10] <- pct_sur;
            resr[i]   <- raw_res;
            eatr[i]   <- raw_eat;
            srvr[i]   <- raw_sur;
        }
        if(i %% 1000 == 0){
            prct_c <- 100 * i / dim(mat)[1];
            print(paste("Summarisation landscape ",prct_c,"% complete"));
        }
    }
    population  <- dim(pest)[1];
    p_genos     <- as.numeric(paste(pest[,5], pest[,6], sep = ""));
    p_genotypes <- length(unique(p_genos));
    c_genos     <- as.numeric(paste(pest[,7], pest[,8], sep = ""));
    c_genotypes <- length(unique(c_genos));
    resr        <- resr[!is.na(resr)];
    eatr        <- eatr[!is.na(eatr)];
    srvr        <- srvr[!is.na(srvr)];
    pct_resist  <- 100 * sum(resr) / population;
    pct_eaters  <- 100 * sum(eatr) / population;
    pct_surviv  <- 100 * sum(srvr) / population;
    landscape   <- c(population, p_genotypes, c_genotypes, pct_resist, 
                     pct_eaters, pct_surviv);
    colnames(data) <- c("ID", "sex", "xloc", "yloc", "path", "crop", 
                        "p_geno", "c_geno", "resist_path", "eat_crop");
    names(landscape) <- c("pop_size", "resist_genotypes", "crop_genotypes", 
                          "percentage_resistant", "percentage_crop_eaters",
                          "percentage_survivors");
    colnames(mat)    <- c("xloc", "yloc", "crop", "pathogen", "pop_size", 
                          "genotypes_resist", "genotypes_crop", 
                          "percentage_resistant", "percentage_crop_eaters",
                          "percentage_survivors");
    sim_results <- list(data = data, landscape = landscape, mat = mat);
    return(sim_results);
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
    return(list(densities = den_vect, pathogen_fr = p_tabl, crop_fr = c_tabl,
                resistance = resist_table));
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


toy_block_land <- function(xdim, ydim, pathogens, crops, block_len){
    cells     <- xdim * ydim;
    cell_left <- cells %% (block_len * block_len);
    if(cell_left > 0){
        stop("Landscape cannot be divided into squares of size 'block_len'");
    }
    LAND      <- array(data = 0, dim = c(xdim, ydim, 3));
    tx0       <- 1;
    tx1       <- block_len;
    ty0       <- 1;
    ty1       <- block_len;
    block_num <- 1;
    while(tx0 <= xdim & ty0 <= ydim){
        LAND[tx0:tx1, ty0:ty1, 1] <- block_num;
        tx0                       <- tx1 + 1;
        tx1                       <- tx1 + block_len;
        if(tx0 > xdim){
            tx0 <- 0;
            tx1 <- block_len;
            ty0 <- ty1 + 1;
            ty1 <- ty1 + block_len;
        }
        block_num <- block_num + 1;
    }
    block_num  <- block_num - 1;
    for(block in 1:block_num){
        rand_path <- sample(x = 1:pathogens, size = 1);
        rand_crop <- sample(x = 1:crops, size = 1);
        for(xx in 1:xdim){
            for(yy in 1:ydim){
                if(LAND[xx, yy, 1] == block){
                    LAND[xx, yy, 2] <- rand_path;
                    LAND[xx, yy, 3] <- rand_crop;
                }
            }
        }
    }
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

# Simulate Poisson movement of toys -- a nasty loop here -- faster in C
toy_pois_move_pest <- function(PEST, LAND, dist = 1){
    pest_n   <- dim(PEST)[1];
    xlim     <- dim(LAND)[2];
    ylim     <- dim(LAND)[1];
    distance <- rpois(n = pest_n, lambda = dist);
    for(pest in 1:pest_n){
        move <- distance[pest];
        while(move > 0){
            move_x  <- sample(x = -move:move, size = 1); 
            move_y  <- sample(x = -move:move, size = 1);
            PEST[pest, 3] <- PEST[pest, 3] + move_x;
            PEST[pest, 4] <- PEST[pest, 4] + move_y;
            move    <- move - 1;
        }
        while(PEST[pest, 3] < 1){
            PEST[pest, 3] <- PEST[pest, 3] + xlim;
        }
        while(PEST[pest, 3] > xlim){
            PEST[pest, 3] <- PEST[pest, 3] - xlim;
        }
        while(PEST[pest, 4] < 1){
            PEST[pest, 4] <- PEST[pest, 4] + ylim;
        }
        while(PEST[pest, 4] > ylim){
            PEST[pest, 4] <- PEST[pest, 4] - ylim;
        }
    }
    return(PEST);
}

# Summarise the distance travelled for all pests
toy_move_distribution <- function(PEST_before, PEST_after, LAND){
    same_dim <- identical( dim(PEST_before), dim(PEST_after) );
    if(same_dim == FALSE){
        stop("The pest array dimensions need to be the same");
    }
    xlim   <- dim(LAND)[2];
    ylim   <- dim(LAND)[1];
    pest_n <- dim(PEST_before)[1];
    pest_d <- rep(x = 0, length = pest_n);
    for(pest in 1:pest_n){
        xdist1  <- (PEST_before[pest, 3] - PEST_after[pest, 3])^2;
        ydist1  <- (PEST_before[pest, 4] - PEST_after[pest, 4])^2;
        xdist2  <- (PEST_before[pest, 3] - PEST_after[pest, 3] - xlim)^2;
        ydist2  <- (PEST_before[pest, 4] - PEST_after[pest, 4] - ylim)^2;
        xdist3  <- (PEST_before[pest, 3] - PEST_after[pest, 3] + xlim)^2;
        ydist3  <- (PEST_before[pest, 4] - PEST_after[pest, 4] + ylim)^2;
        xdist   <- min(c(xdist1, xdist2, xdist3));
        ydist   <- min(c(ydist1, ydist2, ydist3));
        tdist  <- sqrt(xdist + ydist);
        pest_d[pest] <- tdist;
    }
    return(pest_d);
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
    locals   <- sample(x = locals, size = length(locals));
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


