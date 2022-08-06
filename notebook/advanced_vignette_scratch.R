mg  <- mine_gmatrix(gmatrix = gmt, loci = 12, indivs = 2000, npsize = 12000, 
                    max_gen = 5400, sampleK = 1200, chooseK = 6, layers = 1,
                    mu_pr = 0.2, pr_cross = 0.2, mu_sd = 0.004, 
                    term_cri = -8);



library("resevol");
gmt <- matrix(data = c(1.0, -0.5, 0.2, 0.2, -0.5, 1.0, 0.2, 0.2, 0.2, 
                       0.2, 0.4, -0.6, 0.2, 0.2, -0.6, 0.4), nrow = 4);


set.seed(2022);
start <- Sys.time();
mg    <- mine_gmatrix(gmatrix = gmt, loci = 12, indivs = 2000, npsize = 12000, 
                      max_gen = 5400, sampleK = 1200, chooseK = 6, layers = 2,
                      mu_pr = 0.1, pr_cross = 0.1, mu_sd = 0.005, 
                      term_cri = -8, sd_ini = 0.01);
elaps <- Sys.time() - start;
print(elaps);



set.seed(2022);
start <- Sys.time();
mg    <- mine_gmatrix(gmatrix = gmt, loci = 12, indivs = 2000, npsize = 12000, 
                      max_gen = 5400, sampleK = 1200, chooseK = 6, layers = 2,
                      mu_pr = 0.1, pr_cross = 0.1, mu_sd = 0.01, 
                      term_cri = -8);
elaps <- Sys.time() - start;
print(elaps);

set.seed(2022);
start <- Sys.time();
mg    <- mine_gmatrix(gmatrix = gmt, loci = 12, indivs = 2000, npsize = 12000, 
                      max_gen = 5400, sampleK = 1200, chooseK = 6, layers = 2,
                      mu_pr = 0.2, pr_cross = 0.2, mu_sd = 0.02, 
                      term_cri = -8);
elaps <- Sys.time() - start;
print(elaps);

set.seed(2022);
start <- Sys.time();
mg    <- mine_gmatrix(gmatrix = gmt, loci = 12, indivs = 2000, npsize = 12000, 
                      max_gen = 5400, sampleK = 1200, chooseK = 6, layers = 2,
                      mu_pr = 0.05, pr_cross = 0.05, mu_sd = 0.01, 
                      term_cri = -8, sd_ini = 0.01);
elaps <- Sys.time() - start;
print(elaps);














land_file <- system.file("landscape_eg.csv", package = "resevol");
land_dat  <- read.csv(file = land_file, header = FALSE);
land_eg   <- t(as.matrix(land_dat));
farm_cols <- c("#f4eadc", "#6a4b20", "#cea05f", "#e1c59d", "#a97833", "#cea05f",
               "#f2e6d6", "#6a4b20", "#cc9c59", "#dfc197", "#a27331", "#f0e3d0",
               "#5d421c", "#ca9852");
land_cols <- c(farm_cols, "#00ab41", "#234F1E", "#2832C2");
par(mar = c(0, 0, 0, 0));
image(land_eg, xaxt = "n", yaxt = "n", col = land_cols);

last_time_step_file <- system.file("last_time_step.csv", package = "resevol");
pop_last_time_step  <- read.csv(file = last_time_step_file, header = FALSE);
landscape    <- land_eg;
for(i in 1:dim(pop_last_time_step)[1]){
    xloc <- pop_last_time_step[i, 1] + 1;
    yloc <- pop_last_time_step[i, 2] + 1;
    landscape[xloc, yloc] <- 18;
}
land_cols <- c(land_cols, "#000000");














################################
library("resevol");
load(file = "advanced_eg_network.rda");
land_dat  <- read.csv(file = "landscape_eg.csv", header = FALSE);
land_eg   <- t(as.matrix(land_dat));

initial_crop <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3);

rotate_crop       <- matrix(data = 0, nrow = 3, ncol = 3);
rotate_crop[1, 2] <- 1;
rotate_crop[2, 1] <- 1;
rotate_crop[3, 3] <- 1;

initial_pesticide <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3);

rotate_pesticide           <- matrix(data = 0, nrow = 3, ncol = 3);
rotate_pesticide[1, 1]     <- 1;
rotate_pesticide[2, 2]     <- 1;
rotate_pesticide[3, 3]     <- 1;

set.seed(2022);
sim1       <- run_farm_sim(mine_output             = mg, 
                          terrain                  = land_eg,
                          crop_init                = initial_crop,
                          crop_rotation_type       = rotate_crop,
                          pesticide_init           = initial_pesticide,
                          pesticide_rotation_type  = rotate_pesticide,
                          food_consume             = c("T1", "T2", 0),
                          pesticide_consume        = c("T3", "T4", 0),
                          crop_number              = 3,
                          pesticide_number         = 3,
                          trait_means              = c(2, 2, 0.0, 0.0),
                          max_age                  = 6,
                          min_age_feed             = 0,
                          max_age_feed             = 2,
                          min_age_move             = 3,
                          max_age_move             = 6,
                          min_age_metabolism       = 3,
                          max_age_metabolism       = 6,
                          metabolism               = 0.5,
                          food_needed_surv         = 1,
                          reproduction_type        = "food_based",
                          food_needed_repr         = 2,
                          N                        = 1000, 
                          repro                    = "biparental",
                          mating_distance          = 4,
                          rand_age                 = TRUE,
                          pesticide_tolerated_surv = 2,
                          movement_bouts           = 4,
                          move_distance            = 2,
                          crop_per_cell            = 10,
                          crop_sd                  = 0,
                          pesticide_per_cell       = 1,
                          pesticide_sd             = 0,
                          crop_rotation_time       = 18,
                          pesticide_rotation_time  = 9,
                          time_steps               = 240,
                          pesticide_start          = 81,
                          immigration_rate         = 100,
                          land_edge                = "reflect",
                          mutation_pr              = 0.01,
                          crossover_pr             = 0.01,
                          print_gens              = TRUE,
                          print_last              = TRUE);



rotate_pesticide           <- matrix(data = 0, nrow = 3, ncol = 3);
rotate_pesticide[1, 2]     <- 1;
rotate_pesticide[2, 1]     <- 1;
rotate_pesticide[3, 3]     <- 1;
set.seed(2022);
sim2       <- run_farm_sim(mine_output              = mg, 
                           terrain                  = land_eg,
                           crop_init                = initial_crop,
                           crop_rotation_type       = rotate_crop,
                           pesticide_init           = initial_pesticide,
                           pesticide_rotation_type  = rotate_pesticide,
                           food_consume             = c("T1", "T2", 0),
                           pesticide_consume        = c("T3", "T4", 0),
                           crop_number              = 3,
                           pesticide_number         = 3,
                           trait_means              = c(2, 2, 0.0, 0.0),
                           max_age                  = 6,
                           min_age_feed             = 0,
                           max_age_feed             = 2,
                           min_age_move             = 3,
                           max_age_move             = 6,
                           min_age_metabolism       = 3,
                           max_age_metabolism       = 6,
                           metabolism               = 0.5,
                           food_needed_surv         = 1,
                           reproduction_type        = "food_based",
                           food_needed_repr         = 2,
                           N                        = 1000, 
                           repro                    = "biparental",
                           mating_distance          = 4,
                           rand_age                 = TRUE,
                           pesticide_tolerated_surv = 2,
                           movement_bouts           = 4,
                           move_distance            = 2,
                           crop_per_cell            = 10,
                           crop_sd                  = 0,
                           pesticide_per_cell       = 1,
                           pesticide_sd             = 0,
                           crop_rotation_time       = 18,
                           pesticide_rotation_time  = 9,
                           time_steps               = 240,
                           pesticide_start          = 81,
                           immigration_rate         = 100,
                           land_edge                = "reflect",
                           mutation_pr              = 0.01,
                           crossover_pr             = 0.01,
                           print_gens              = TRUE,
                           print_last              = TRUE);



# Sim 3
initial_pesticide <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 3, 3, 3);
set.seed(2022);
sim3       <- run_farm_sim(mine_output              = mg, 
                           terrain                  = land_eg,
                           crop_init                = initial_crop,
                           crop_rotation_type       = rotate_crop,
                           pesticide_init           = initial_pesticide,
                           pesticide_rotation_type  = rotate_pesticide,
                           food_consume             = c("T1", "T2", 0),
                           pesticide_consume        = c("T3", "T4", 0),
                           crop_number              = 3,
                           pesticide_number         = 3,
                           trait_means              = c(2, 2, 0.0, 0.0),
                           max_age                  = 6,
                           min_age_feed             = 0,
                           max_age_feed             = 2,
                           min_age_move             = 3,
                           max_age_move             = 6,
                           min_age_metabolism       = 3,
                           max_age_metabolism       = 6,
                           metabolism               = 0.5,
                           food_needed_surv         = 1,
                           reproduction_type        = "food_based",
                           food_needed_repr         = 2,
                           N                        = 1000, 
                           repro                    = "biparental",
                           mating_distance          = 4,
                           rand_age                 = TRUE,
                           pesticide_tolerated_surv = 2,
                           movement_bouts           = 4,
                           move_distance            = 2,
                           crop_per_cell            = 10,
                           crop_sd                  = 0,
                           pesticide_per_cell       = 1,
                           pesticide_sd             = 0,
                           crop_rotation_time       = 18,
                           pesticide_rotation_time  = 9,
                           time_steps               = 240,
                           pesticide_start          = 81,
                           immigration_rate         = 100,
                           land_edge                = "reflect",
                           mutation_pr              = 0.01,
                           crossover_pr             = 0.01,
                           print_gens              = TRUE,
                           print_last              = TRUE);



