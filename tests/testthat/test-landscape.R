library("testthat");
library("resevol");
context("landscape tests");

test_that("Landscape dimensions are correct", {
    skip_on_cran();
    land <- make_landscape(rows = 12, cols = 12, farms = 12);
    expect_equal(dim(land), c(12, 12, 21));
})

test_that("Number of farms is correct", {
    skip_on_cran();
    land     <- make_landscape(rows = 12, cols = 12, farms = 12);
    farms    <- length(unique(as.vector(land[,,1])));
    expect_equal(farms, 12);
})

test_that("Landscape farm allocation is equal", {
    skip_on_cran();
    land     <- make_landscape(rows = 12, cols = 12, farms = 12);
    farms    <- length(unique(as.vector(land[,,1])));
    farmland <- table(as.vector(land[,,1]));
    expect_equal(as.vector(farmland), rep(12, farms));
})

test_that("Custom landscape can be added", {
    skip_on_cran();
    custom_terrain       <- matrix(data = 0, nrow = 8, ncol = 8);
    custom_terrain[,1:2] <- 1;
    custom_terrain[,2:6] <- 2;
    custom_terrain[,7:8] <- 3;
    land <- make_landscape(terrain = custom_terrain);
    expect_equal(dim(land), c(8, 8, 21));
})

test_that("Crops rotate correctly over time", {
    skip_on_cran();
    gmt       <- matrix(data = 0, nrow = 2, ncol = 2);
    diag(gmt) <- 1;
    mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 2, indivs = 100, 
                              npsize = 100, max_gen = 4, prnt_out = FALSE);
    c_init    <- 1;
    sim       <- run_farm_sim(mine_output = mg, N = 100, xdim = 8, ydim = 8, 
                              repro = "asexual", neutral_loci = 10, 
                              max_age = 9, min_age_move = 0, max_age_move = 9,
                              min_age_reproduce = 0, max_age_reproduce = 9, 
                              min_age_feed = 0, max_age_feed = 9,
                              food_consume = 0.0, pesticide_consume = 0.0,
                              rand_age = FALSE, move_distance = 1, 
                              food_needed_surv = 0.00, 
                              pesticide_tolerated_surv = 0.1, 
                              food_needed_repr = 0,
                              pesticide_tolerated_repr = 0,
                              reproduction_type = "lambda", mating_distance = 1,
                              lambda_value = 1,
                              movement_bouts = 1, crop_init = c_init,
                              selfing = TRUE, feed_while_moving = FALSE,
                              mortality_type = 0, age_food_threshold = 0,
                              age_pesticide_threshold = 0, farms = 1,
                              time_steps = 4, mutation_pr = 0,
                              crossover_pr = 0, mutation_type = 0, 
                              net_mu_layers = 0, net_mu_dir = 0, 
                              mutation_direction = 0, crop_rotation_type = 3,
                              crop_rotation_time = 1, 
                              pesticide_rotation_type = 3, 
                              pesticide_rotation_time = 1, crop_per_cell = 1,
                              pesticide_per_cell = 1, crop_sd = 0, 
                              pesticide_sd = 0, crop_min = 0, crop_max = 1000,
                              pesticide_min = 0, pesticide_max = 1000,
                              crop_number = 5, pesticide_number = 1, 
                              print_inds = FALSE, print_gens = FALSE,
                              print_last = FALSE, K_on_birth = 1000,
                              pesticide_start = 0, immigration_rate = 0,
                              get_f_coef = FALSE, get_stats = FALSE,
                              metabolism = 0);
    crop_pos <- sum(sim[[2]][,,6]); crop_pos;
    expect_equal(crop_pos, 64);
})

test_that("Pesticides rotate correctly over time", {
    skip_on_cran();
    gmt       <- matrix(data = 0, nrow = 2, ncol = 2);
    diag(gmt) <- 1;
    mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 2, indivs = 100, 
                              npsize = 100, max_gen = 4, prnt_out = FALSE);
    p_init    <- 1;
    sim       <- run_farm_sim(mine_output = mg, N = 100, xdim = 8, ydim = 8, 
                              repro = "asexual", neutral_loci = 10, 
                              max_age = 9, min_age_move = 0, max_age_move = 9,
                              min_age_reproduce = 0, max_age_reproduce = 9, 
                              min_age_feed = 0, max_age_feed = 9,
                              food_consume = 0.0, pesticide_consume = 0.0,
                              rand_age = FALSE, move_distance = 1, 
                              food_needed_surv = 0.00, 
                              pesticide_tolerated_surv = 0.1, 
                              food_needed_repr = 0,
                              pesticide_tolerated_repr = 0,
                              reproduction_type = "lambda", mating_distance = 1,
                              lambda_value = 1,
                              movement_bouts = 1, pesticide_init = p_init,
                              selfing = TRUE, feed_while_moving = FALSE,
                              mortality_type = 0, age_food_threshold = 0,
                              age_pesticide_threshold = 0, farms = 1,
                              time_steps = 4, mutation_pr = 0,
                              crossover_pr = 0, mutation_type = 0, 
                              net_mu_layers = 0, net_mu_dir = 0, 
                              mutation_direction = 0, crop_rotation_type = 3,
                              crop_rotation_time = 1, 
                              pesticide_rotation_type = 3, 
                              pesticide_rotation_time = 1, crop_per_cell = 1,
                              pesticide_per_cell = 1, crop_sd = 0, 
                              pesticide_sd = 0, crop_min = 0, crop_max = 1000,
                              pesticide_min = 0, pesticide_max = 1000,
                              crop_number = 1, pesticide_number = 5, 
                              print_inds = FALSE, print_gens = FALSE,
                              print_last = FALSE, K_on_birth = 1000,
                              pesticide_start = 0, immigration_rate = 0,
                              get_f_coef = FALSE, get_stats = FALSE,
                              metabolism = 0);
    pesticide_pos <- sum(sim[[2]][,,16]);
    expect_equal(pesticide_pos, 64);
})


