library("helicoverpa");
set.seed(Sys.time());
mg  <- readRDS("mg.rds");
sim <- run_farm_sim(mine_output              = mg,
                    N                        = 1000,
                    neutral_loci             = 1000,
                    xdim                     = 144,
                    ydim                     = 144,
                    repro                    = "biparental",
                    max_age                  = 6,
                    selfing                  = FALSE,
                    food_consume             = c("T1", "T2"),
                    pesticide_consume        = c("T3", "T4"),
                    food_needed_surv         = 1,
                    food_needed_repr         = 1,
                    reproduction_type        = "food_based",
                    pesticide_tolerated_surv = 0,
                    pesticide_rotation_type  = 1,
                    crop_rotation_type       = 1,
                    min_age_reproduce        = 4,
                    max_age_feed             = 3,
                    lambda_value             = 1.5,
                    farms                    = 24,
                    time_steps               = 480,
                    mutation_pr              = 0.001,
                    crossover_pr             = 0.1,
                    net_mu_layers            = 0,
                    crop_rotation_time       = 12,
                    pesticide_rotation_time  = 12,
                    crop_per_cell            = 8,
                    pesticide_per_cell       = 0.4,
                    crop_number              = 2,
                    pesticide_number         = 2,
                    print_inds               = FALSE, # Careful with this one
                    K_on_birth               = 1000000,
                    min_age_move             = 4,
                    age_food_threshold       = 2,
                    min_age_feed             = 1,
                    pesticide_start          = 100,
                    print_last               = TRUE,
                    mating_distance          = 2,
                    immigration_rate         = 0
);

# Read in the results files
loc <- read.csv("last_time_step.csv", header = FALSE);
dat <- read.csv("population_data.csv");

# Plot the ending landscape
lnd <- sim[[2]];
tco <- sample(hcl.colors(24, "terrain"), 24, FALSE);
image(lnd[,,1], xaxt = "n", yaxt = "n", col = tco);
j1 <- runif(n = dim(loc)[1], min = -0.0002, max = 0.0002);
j2 <- runif(n = dim(loc)[1], min = -0.0002, max = 0.0002);
points(x = (1/143)*(loc[,3]) + j1, y = (1/143)*(loc[,4]) + j2, pch = 20);

# Plot the population trajectory
par(mfrow = c(2, 2));
plot(x = dat$time_step, y = dat$population_size, 
     xlab = "Time", ylab = "Population Size", type = "l",
     lwd = 2);
plot(x = dat$time_step, y = dat$trait1_mean_value, 
     xlab = "Time", ylab = "Trait 1 mean", type = "l",
     lwd = 2);
plot(x = dat$time_step, y = dat$trait2_mean_value, 
     xlab = "Time", ylab = "Trait 2 mean", type = "l",
     lwd = 2);
plot(x = dat$time_step, y = dat$mean_food_consumed, 
     xlab = "Time", ylab = "Mean food consumed", type = "l",
     lwd = 2);

# Plot trait covariances
par(mfrow = c(3, 2));
# Panel 1
j1 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
j2 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
plot(x = loc[, 102], y = loc[, 103], xlab = "Crop 1 Consumption", 
     ylab = "Crop 2 Consumption");
abline(h = 0, col = "red");
abline(v = 0, col = "red");
# Panel 2
j1 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
j2 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
plot(x = loc[, 102], y = loc[, 104], xlab = "Crop 1 Consumption", 
     ylab = "Pesticide 1 Consumption");
abline(h = 0, col = "red");
abline(v = 0, col = "red");
# Panel 3
j1 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
j2 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
plot(x = loc[, 102], y = loc[, 105], xlab = "Crop 1 Consumption", 
     ylab = "Pesticide 2 Consumption");
abline(h = 0, col = "red");
abline(v = 0, col = "red");
# Panel 4
j1 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
j2 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
plot(x = loc[, 103], y = loc[, 104], xlab = "Crop 2 Consumption", 
     ylab = "Pesticide 1 Consumption");
abline(h = 0, col = "red");
abline(v = 0, col = "red");
# Panel 5
j1 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
j2 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
plot(x = loc[, 103], y = loc[, 105], xlab = "Crop 2 Consumption", 
     ylab = "Pesticide 2 Consumption");
abline(h = 0, col = "red");
abline(v = 0, col = "red");
# Panel 6
j1 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
j2 <- runif(n = dim(loc)[1], min = -0.05, max = 0.05);
plot(x = loc[, 104], y = loc[, 105], xlab = "Pesticide 1 Consumption", 
     ylab = "Pesticide 2 Consumption");
abline(h = 0, col = "red");
abline(v = 0, col = "red");









