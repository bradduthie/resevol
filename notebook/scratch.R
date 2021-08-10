################################################################################
# Example with two traits
################################################################################
library(helicoverpa);
gmt <- matrix(data = 0, nrow = 4, ncol = 4);
diag(gmt) <- 1;
gmt[1, 2] <-  -0.5;
gmt[2, 1] <-  -0.5;
gmt[1, 3] <-  0.2;
gmt[1, 4] <-  0.2;
gmt[3, 1] <-  0.2;
gmt[4, 1] <-  0.2;
gmt[2, 3] <-  0.2;
gmt[2, 4] <-  0.2;
gmt[3, 2] <-  0.2;
gmt[4, 2] <-  0.2;
gmt[3, 4] <- -0.5;
gmt[4, 3] <- -0.5;
gmt[3, 3] <-  1.0;
gmt[4, 4] <-  1.0;
mg  <- mine_gmatrix(gmatrix = gmt, loci = 12, indivs = 2000, npsize = 12000, 
                    max_gen = 2400, sampleK = 1200, chooseK = 6, layers = 6,
                    mu_pr = 0.05, pr_cross = 0.05, mu_sd = 0.01, 
                    term_cri = -12);


mg  <- readRDS("mg.rds");
sim <- run_farm_sim(mine_output              = mg,
                    N                        = 1000,
                    neutral_loci             = 1000,
                    xdim                     = 144,
                    ydim                     = 144,
                    repro                    = "sexual",
                    max_age                  = 4,
                    selfing                  = FALSE,
                    food_consume             = c("T1", "T2"),
                    pesticide_consume        = c("T3", "T4"),
                    food_needed_surv         = 2,
                    food_needed_repr         = 2,
                    reproduction_type        = "food_based",
                    pesticide_tolerated_surv = 0,
                    pesticide_rotation_type  = 2,
                    crop_rotation_type       = 2,
                    min_age_reproduce        = 4,
                    max_age_feed             = 2,
                    lambda_value             = 1.5,
                    farms                    = 24,
                    time_steps               = 400,
                    mutation_pr              = 0.001,
                    crossover_pr             = 0.1,
                    net_mu_layers            = 0,
                    crop_rotation_time       = 12,
                    pesticide_rotation_time  = 12,
                    crop_per_cell            = 2,
                    pesticide_per_cell       = 2,
                    crop_number              = 2,
                    pesticide_number         = 2,
                    print_inds               = FALSE, # Careful with this one
                    K_on_birth               = 20736,
                    min_age_move             = 3,
                    age_food_threshold       = 2,
                    min_age_feed             = 1,
                    pesticide_start          = 200,
                    print_last               = TRUE,
                    immigration_rate         = 0
);

# gzip -v9 data.csv
# gunzip -v data.csv.gz

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# Initialising a file
#library(helicoverpa)

gmt <- matrix(data = 0, nrow = 2, ncol = 2);
diag(gmt) <- 1;
gmt[1, 2] <- -0.5;
gmt[2, 1] <- -0.5;
mg  <- mine_gmatrix(gmatrix = gmt, loci = 6, indivs = 1000, npsize = 8000, 
                    max_gen = 6, sampleK = 400, chooseK = 4, layers = 2);
land  <- make_landscape(rows = 10, cols = 10, depth = 21, farms = 4);
pests <- initialise_inds(mine_output = mg, N = 1000, neutral_loci = 1000, 
                         xdim = 4, ydim = 4, repro = "biparental", max_age = 4, 
                         selfing = FALSE, food_consume = 0.025, 
                         pesticide_consume = 0.0, food_needed_surv = 0.025,
                         min_age_reproduce = 2, lambda_value = 1.3);
tt    <- sim_crops(pests, land, time_steps = 20, mutation_pr = 0.01,
                   crossover_pr = 0.01, net_mu_layers = 0,
                   crop_rotation_time = 4, pesticide_rotation_time = 4,
                   crop_per_cell = 1, pesticide_per_cell = 1, 
                   crop_number = 1, pesticide_number = 1, print_inds = FALSE,
                   K_on_birth = 1000000, print_last = FALSE);




# NEXT STEPS:


inds <- read.csv("last_time_step.csv", header = FALSE);






pests <- initialise_inds(mine_output = mg, N = 10, neutral_loci = 100);
write.csv(pests, "notebook/pests.csv", row.names = FALSE);


pests <- read.csv("notebook/pests.csv");


pests <- initialise_inds(mine_output = mg, N = 10, neutral_loci = 100,
                         repro = "asexual");
land  <- make_landscape(rows = 10, cols = 10, depth = 2, farms = 100);

sim_crops(pests, land);


pests <- initialise_inds(mine_output = mg, N = 10, neutral_loci = 100, 
                         xdim = 2, ydim = 2, repro = "asexual");
tt <- sim_crops(pests, land);


custcol <- sample(x = 1:100, size = 100, replace = FALSE);
for(i in 1:100){
    land[land[,,1] == i] <- custcol[i] + 100; 
}
land[,,1] < land[,,1] - 100;



# mat <- matrix(data = 0, nrow = 8, ncol = 8);
# diag(mat) <- 1;

mat <- read.csv("notebook/gmat.csv");
gmt <- as.matrix(mat);
mg  <- mine_gmatrix(gmatrix = gmt, indivs  = 10000, npsize = 12000, 
                    max_gen = 2000, sampleK = 3200, chooseK = 40, term_cri = -7, 
                    use_cor = TRUE);


#==============================================================================#
#   IMPORTANT THINGS TO DO WITH THE mine_gmatrix FUNCTION
#==============================================================================#
# Scale the variation of the initialised network to the gmatrix input.
# Either that, or allow the variation to be set in the R function
# Maybe set it to the digit just below the highest (e.g., 1 then 0.1, 10 then 1)
#
# Have an option for finding the correlation matrix rather that VCV
# Figure out how to scale the trait values appropriately
#
# Have an option for diploid genetic architecture
#==============================================================================#



N      <- 100000;
loci   <- 18;
inds   <- matrix(data = rnorm(n = (N*loci), sd = 1), nrow = N, ncol = loci);
trts   <- inds %*% mg[[5]];
cov(trts);

# Gen: 11999	 Stress: -1.809979	 Min: -4.980156

vcv <- mg[[2]];
cvs <- cor(trts);
N   <- (9 * 9) - (0.5*(9)*(9-1));
val <- 0;
for(i in 1:9){
    for(j in 1:9){
        if(j <= i){
            val <- val + ((vcv[i,j] - cvs[i,j]) * (vcv[i,j] - cvs[i,j]) / N);
        }
    }
}


mg[[3]] %*% mg[[4]][,,1] %*% mg[[4]][,,2] %*% mg[[4]][,,3];



mat <- trts;

mns <- rep(0, dim(mat)[2]);
for(i in 1:dim(mat)[2]){
    mns[i] <- 0;
    for(j in 1:dim(mat)[1]){
        mns[i] <- mat[j,i];
    }
    mns[i] <- mns[i] / dim(mat)[1];
}

VCV <- matrix(data = 0, nrow = dim(mat)[2], ncol = dim(mat)[2]);
for(i in 1:dim(mat)[2]){
    for(j in 1:dim(mat)[2]){
         VCV[i, j] <- 0;
         VCV[j, i] <- 0;
         for(k in 1:dim(mat)[1]){
             VCV[i,j] <- VCV[i, j] + (mat[k,i] - mns[i]) * (mat[k,j] - mns[j]);
         }
         VCV[i, j] <- VCV[i, j] / (dim(mat)[1] - 1);
         VCV[j, i] <- VCV[i, j];
    }
}



###############
### Getting the T1 trait notation to work:
assign_traits <- function(paras, move_distance, food_needed_surv,
                          pesticide_tol_surv, food_needed_repr, 
                          pesticide_tol_repr, mating_distance, lambda_value){
    
    trait_start_c  <- paras[109];
    move_distance_T <- check_is_trait(move_distance);
    if(move_distance_T == TRUE){
        move_distance_col <- get_trait_number(move_distance) + trait_start_c;
        paras[5]          <- move_distance_col;
    }
    
    food_needed_T <- check_is_trait(food_needed_surv);
    if(food_needed_surv_T == TRUE){
        food_needed_col <- get_trait_number(food_needed_surv) + trait_start_c;
        paras[16]       <- food_needed_col;
    }
    
    pesticide_tol_surv_T <- check_is_trait(pesticide_tol_surv);
    if(pesticide_tol_surv_T == TRUE){
        pest_tol_col <- get_trait_number(pesticide_tol_surv) + trait_start_c;
        paras[17]    <- pest_tol_col;
    }
    
    food_needed_rpr_T <- check_is_trait(food_needed_repr);
    if(food_needed_rpr_T == TRUE){
        food_repr_col  <- get_trait_number(food_needed_repr) + trait_start_c;
        paras[18]      <- food_repr_col;
    }
    
    pest_tol_rpr_T <- check_is_trait(pesticide_tol_repr);
    if(pest_tol_rpr_T == TRUE){
        pest_repr_col  <- get_trait_number(pesticide_tol_repr) + trait_start_c;
        paras[19]      <- pest_repr_col;
    }
    
    mating_dist_T <- check_is_trait(mating_distance);
    if(mating_dist_T == TRUE){
        mate_dist_col  <- get_trait_number(mating_distance) + trait_start_c;
        paras[24]      <- mate_dist_col;
    }
    
    lambda_T <- check_is_trait(lambda_value);
    if(lambda_T == TRUE){
        lambda_col    <- get_trait_number(lambda_value) + trait_start_c;
        paras[25]     <- lambda_col;
    }
    
    
    return(paras);
}





##########################3


sim <- run_farm_sim(mine_output              = mg,
                    N                        = 10000,
                    neutral_loci             = 1000,
                    xdim                     = 100,
                    ydim                     = 100,
                    repro                    = "biparental",
                    max_age                  = 10,
                    selfing                  = FALSE,
                    food_consume             = c("T1", "T2"),
                    pesticide_consume        = c("T3", "T4"),
                    food_needed_surv         = 1,
                    food_needed_repr         = 1,
                    reproduction_type        = "food_based",
                    pesticide_tolerated_surv = 0,
                    pesticide_rotation_type  = 2,
                    crop_rotation_type       = 2,
                    min_age_reproduce        = 8,
                    max_age_feed             = 6,
                    lambda_value             = 1.5,
                    farms                    = 16,
                    time_steps               = 600,
                    mutation_pr              = 0.01,
                    crossover_pr             = 0.01,
                    net_mu_layers            = 0,
                    crop_rotation_time       = 24,
                    pesticide_rotation_time  = 24,
                    crop_per_cell            = 2,
                    pesticide_per_cell       = 2,
                    crop_number              = 2,
                    pesticide_number         = 2,
                    K_on_birth               = 1000000,
                    min_age_move             = 8,
                    age_food_threshold       = 7,
                    min_age_feed             = 1,
                    pesticide_start          = 200,
);


