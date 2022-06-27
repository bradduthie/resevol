library("testthat");
library("resevol");
context("Simulation run tests");

test_that("Simulations are run and output is produced", {
    skip_on_cran();
    gmt       <- matrix(data = 0, nrow = 2, ncol = 2);
    diag(gmt) <- 1;
    mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 2, indivs = 100, 
                              npsize = 100, max_gen = 4, prnt_out = FALSE);
    sim       <- run_farm_sim(mine_output = mg, N = 100, xdim = 40, ydim = 40, 
                              repro = "sexual", neutral_loci = 1000, 
                              max_age = 9, min_age_move = 0, max_age_move = 9,
                              min_age_reproduce = 0, max_age_reproduce = 9, 
                              min_age_feed = 0, max_age_feed = 9,
                              food_consume = 0.25, pesticide_consume = 0.1,
                              rand_age = FALSE, move_distance = 1, 
                              food_needed_surv = 0.25, 
                              pesticide_tolerated_surv = 0.1, 
                              food_needed_repr = 0,
                              pesticide_tolerated_repr = 0,
                              reproduction_type = "lambda", mating_distance = 1,
                              lambda_value = 1,
                              movement_bouts = 1,
                              selfing = TRUE, feed_while_moving = FALSE,
                              mortality_type = 0, age_food_threshold = 0,
                              age_pesticide_threshold = 0, farms = 4,
                              time_steps = 4, mutation_pr = 0,
                              crossover_pr = 0, mutation_type = 0, 
                              net_mu_layers = 0, net_mu_dir = 0, 
                              mutation_direction = 0, crop_rotation_type = 2,
                              crop_rotation_time = 1, 
                              pesticide_rotation_type = 2, 
                              pesticide_rotation_time = 1, crop_per_cell = 1,
                              pesticide_per_cell = 1, crop_sd = 0, 
                              pesticide_sd = 0, crop_min = 0, crop_max = 1000,
                              pesticide_min = 0, pesticide_max = 1000,
                              crop_number = 2, pesticide_number = 1, 
                              print_inds = FALSE, print_gens = FALSE,
                              print_last = FALSE, K_on_birth = 1000,
                              pesticide_start = 0, immigration_rate = 0,
                              get_f_coef = FALSE, get_stats = FALSE,
                              metabolism = 0);
    expect_equal(length(sim[[1]]), 173);
    expect_equal(dim(sim[[2]]), c(40, 40, 21));
})







