#' Add column names to individuals output
#'
#' Add column names to the individual-level output CSV files made by the 
#' run_farm_sim function. These files are printed out by run_farm_sim whenever
#' 'print_inds = TRUE' or 'print_last = TRUE', which cause CSV files to be
#' printed to 'individuals.csv' and 'last_time_step.csv', respectively. The
#' latter filename can be customised to something else with the 
#' last_step_filename argument in run_farm_sim. These are large CSV files, and 
#' because of the way simulations must be set up to allow for varying genome
#' sizes and trait numbers, column number is not always the same This
#' function individuals_colnames adds column names to a CSV file of individuals
#' output from 'run_farm_sim'.
#'
#'@param last_step_filename Output of individuals from run_farm_sim function
#'@param mine_output The output from mine_gmatrix, which will be used to
#' initialise the genomes and traits of pests.
#'@return Returns a success message and overwrites CSV file with headers
#'@examples
#'gmt       <- matrix(data = 0, nrow = 4, ncol = 4);
#'diag(gmt) <- 1;
#'mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 3, indivs = 100, 
#'                          npsize = 100, max_gen = 2, prnt_out = FALSE);
#'individuals_colnames("last_time_step.csv", gmt);
#'@useDynLib resevol
#'@importFrom stats rnorm rpois runif
#'@keywords internal
#'@export
individuals_colnames <- function(last_step_filename, mine_output){
    
    file_exists <- file.exists(last_step_filename);
    if(file_exists == FALSE){
        out_message <- paste("No file named", last_step_filename);
        return(out_message);
    }
    
    dat <- read.csv(last_step_filename, header = TRUE);
    if(is.numeric(dat[1, 1]) == FALSE){
        stop("ERROR: It looks like this file already has column names.")
    }

    ind_dim    <- dim(dat);
    ind_dims   <- length(ind_dim);
    if(ind_dims != 2){
        return(dat);
    }
    if(ind_dim[1] < 1){
        return(dat);
    }
    
    inds       <- rep(x = NA, times = ind_dim[2]);
    
    inds[1]    <- "time";
    inds[2]    <- "ID";
    inds[3]    <- "xloc";
    inds[4]    <- "yloc";
    inds[5]    <- "age";
    inds[6]    <- "sex";
    inds[7]    <- "move_dist";
    inds[8]    <- "mumID";
    inds[9]    <- "dadID";
    inds[10]   <- "mum_row";
    inds[11]   <- "dad_row";
    inds[12]   <- "offspring";
    inds[13]   <- "loci";
    inds[14]   <- "traits";
    inds[15]   <- "network_layers";
    inds[16]   <- "food_consumed";
    inds[17]   <- "pesticide_consumed";
    inds[18]   <- "food_needed_surv";
    inds[19]   <- "pesticide_tolerated_surv";
    inds[20]   <- "food_needed_repr";
    inds[21]   <- "pesticide_tolerated_repr";
    inds[22]   <- "tag_1";
    inds[23]   <- "tag_2";
    inds[24]   <- "tag_3";
    inds[25]   <- "repr_type";
    inds[26]   <- "mate_distance";
    inds[27]   <- "repr_parameter";
    inds[28]   <- "selfing_allowed";
    inds[29]   <- "mate_acccessed";
    inds[30]   <- "ploidy";
    inds[31]   <- "neutral_alleles";
    inds[32]   <- "move_bouts";
    inds[33]   <- "min_move_age";
    inds[34]   <- "max_move_age";
    inds[35]   <- "min_feed_age";
    inds[36]   <- "max_feed_age";
    inds[37]   <- "min_repr_age";
    inds[38]   <- "max_repr_age";
    inds[39]   <- "food_1_eats";
    inds[40]   <- "food_2_eats";
    inds[41]   <- "food_3_eats";
    inds[42]   <- "food_4_eats";
    inds[43]   <- "food_5_eats";
    inds[44]   <- "food_6_eats";
    inds[45]   <- "food_7_eats";
    inds[46]   <- "food_8_eats";
    inds[47]   <- "food_9_eats";
    inds[48]   <- "food_10_eats";
    inds[49]   <- "pesticide_1_imbibes";
    inds[50]   <- "pesticide_2_imbibes";
    inds[51]   <- "pesticide_3_imbibes";
    inds[52]   <- "pesticide_4_imbibes";
    inds[53]   <- "pesticide_5_imbibes";
    inds[54]   <- "pesticide_6_imbibes";
    inds[55]   <- "pesticide_7_imbibes";
    inds[56]   <- "pesticide_8_imbibes";
    inds[57]   <- "pesticide_9_imbibes";
    inds[58]   <- "pesticide_10_imbibes";
    inds[59]   <- "eats_during_bout";
    inds[60]   <- "food_1_consumed";
    inds[61]   <- "food_2_consumed";
    inds[62]   <- "food_3_consumed";
    inds[63]   <- "food_4_consumed";
    inds[64]   <- "food_5_consumed";
    inds[65]   <- "food_6_consumed";
    inds[66]   <- "food_7_consumed";
    inds[67]   <- "food_8_consumed";
    inds[68]   <- "food_9_consumed";
    inds[69]   <- "food_10_consumed";
    inds[70]   <- "pesticide_1_consumed";
    inds[71]   <- "pesticide_2_consumed";
    inds[72]   <- "pesticide_3_consumed";
    inds[73]   <- "pesticide_4_consumed";
    inds[74]   <- "pesticide_5_consumed";
    inds[75]   <- "pesticide_6_consumed";
    inds[76]   <- "pesticide_7_consumed";
    inds[77]   <- "pesticide_8_consumed";
    inds[78]   <- "pesticide_9_consumed";
    inds[79]   <- "pesticide_10_consumed";
    inds[80]   <- "pesticide_during_bout";
    inds[81]   <- "mortality_type";
    inds[82]   <- "max_age";
    inds[83]   <- "died";
    inds[84]   <- "age_food_threshold";
    inds[85]   <- "age_pesticide_threshold";
    inds[86]   <- "inbreeding_coefficient";
    inds[87]   <- "lambda_adjust";
    inds[88]   <- "metabolic_rate";
    inds[89]   <- "baseline_metablism";
    inds[90]   <- "min_age_metabolism";
    inds[91]   <- "max_age_metabolism";
    inds[92]   <- "ini_mean_trait_1";
    inds[93]   <- "ini_mean_trait_2";
    inds[94]   <- "ini_mean_trait_3";
    inds[95]   <- "ini_mean_trait_4";
    inds[96]   <- "ini_mean_trait_5";
    inds[97]   <- "ini_mean_trait_6";
    inds[98]   <- "ini_mean_trait_7";
    inds[99]   <- "ini_mean_trait_8";
    inds[100]  <- "ini_mean_trait_9";
    inds[101]  <- "ini_mean_trait_10";
    
    
    loci       <- mine_output[[1]][1];
    layers     <- mine_output[[1]][2];
    traits     <- dim(mine_output[[2]])[1];
    genome     <- mine_output[[7]];
    ploidy     <- dat[1, 30];
    
    if(ploidy == 1){
        trait_start_col   <- 102;
        layers_start_col  <- trait_start_col + traits;
        loci_start_col    <- layers_start_col + layers + 2;
        genome_start_col  <- loci_start_col + loci;
        genome_end_col    <- genome_start_col + length(genome) - 1;
        
        trait_cols   <- trait_start_col:(layers_start_col - 1);
        layers_cols  <- layers_start_col:(loci_start_col - 1);
        loci_cols    <- loci_start_col:(genome_start_col - 1);
        genome_cols  <- genome_start_col:genome_end_col;
        neutral_cols <- (genome_end_col + 1):ind_dim[2];
        
        t_col        <- paste("trait_", 1:traits, sep = "");
        n_col        <- paste("net_pos_", 1:length(layers_cols), sep = "");
        l_col        <- paste("locus_", 1:loci, sep = "");
        v_col        <- paste("N", 1:length(genome_cols), sep = "");
        u_col        <- paste("neutral_", 1:length(neutral_cols), sep = "");
        
        inds[trait_cols]     <- t_col;
        inds[layers_cols]    <- n_col;
        inds[loci_cols]      <- l_col;
        inds[genome_cols]    <- v_col;
        inds[neutral_cols]   <- u_col;
        inds[ind_dim[2]]     <- "empty";
    }
    
    if(ploidy == 2){
        trait_start_col   <- 102;
        layers_start_col  <- trait_start_col + traits;
        loci_start_col    <- layers_start_col + layers + 3;
        genome_start_col  <- loci_start_col + (2 * loci);
        genome_end_col    <- genome_start_col + length(genome);
        dip_geno_end_col  <- genome_start_col + (2 * length(genome)) - 1;
        
        trait_cols   <- trait_start_col:(layers_start_col - 1);
        layers_cols  <- layers_start_col:(loci_start_col - 1);
        loci_cols    <- loci_start_col:(genome_start_col - 1);
        genome_cols  <- genome_start_col:dip_geno_end_col;
        neutral_cols <- (dip_geno_end_col + 1):ind_dim[2];
        
        t_col        <- paste("trait_", 1:traits, sep = "");
        n_col        <- paste("net_pos_", 1:length(layers_cols), sep = "");
        
        l_col        <- rep(x = NA, times = 2 * loci);
        lcount       <- 1;
        for(i in 1:(2*loci)){
            if(i %% 2 != 0){
                l_col[i] <- paste("locus_", lcount, "A", sep = "");
            }else{
                l_col[i] <- paste("locus_", lcount, "B", sep = "");
                lcount   <- lcount + 1;
            }
        }
        
        v_col        <- rep(x = NA, times = length(genome_cols));
        vcount       <- 1;
        for(i in 1:length(v_col)){
            if(i %% 2 != 0){
                v_col[i] <- paste("N", vcount, "A", sep = "");
            }else{
                v_col[i] <- paste("N", vcount, "B", sep = "");
                vcount   <- vcount + 1;
            }
        }
        
        u_col        <- rep(x = NA, times = length(neutral_cols));
        ucount       <- 1;
        for(i in 1:length(u_col)){
            if(i %% 2 != 0){
                u_col[i] <- paste("neutral_", ucount, "A", sep = "");
            }else{
                u_col[i] <- paste("neutral_", ucount, "B", sep = "");
                ucount   <- ucount + 1;
            }
        }
        
        inds[trait_cols]     <- t_col;
        inds[layers_cols]    <- n_col;
        inds[loci_cols]      <- l_col;
        inds[genome_cols]    <- v_col;
        inds[neutral_cols]   <- u_col;
        inds[ind_dim[2]]     <- "empty";
        
    }
    
    colnames(dat) <- inds;
    
    write.csv(x = dat, file = last_step_filename, row.names = FALSE);
    
    return("Successfully added columns.");
}


