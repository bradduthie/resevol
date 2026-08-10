library("testthat");
library("resevol");
context("edge_effect break statements");
set.seed(1);

# No reproduction (lambda_value = 0) so survivors can only leave via the edges.
# With move_distance = 50 on a 10x10 grid, every displacement leaves the
# landscape, so leaky behaviour is deterministic: everyone dies off-edge.
edge_args <- list(N = 5, xdim = 10, ydim = 10, repro = "asexual",
                  neutral_loci = 10, max_age = 100,
                  min_age_move = 0, max_age_move = 100,
                  min_age_reproduce = 0, max_age_reproduce = 100,
                  min_age_feed = 0, max_age_feed = 100, food_consume = 0,
                  pesticide_consume = 0, rand_age = FALSE, move_distance = 50,
                  food_needed_surv = 0, pesticide_tolerated_surv = 100,
                  food_needed_repr = 1000, pesticide_tolerated_repr = 0,
                  reproduction_type = "lambda", mating_distance = 0,
                  lambda_value = 0, movement_bouts = 1, selfing = TRUE,
                  feed_while_moving = FALSE, pesticide_while_moving = FALSE,
                  mortality_type = 0, age_food_threshold = 0,
                  age_pesticide_threshold = 0, farms = 4, time_steps = 1,
                  mutation_pr = 0, crossover_pr = 0, mutation_type = 0,
                  net_mu_layers = 0, net_mu_dir = 0, mutation_direction = 0,
                  crop_rotation_type = 2, crop_rotation_time = 1,
                  pesticide_rotation_type = 2, pesticide_rotation_time = 1,
                  crop_per_cell = 1, pesticide_per_cell = 1, crop_sd = 0,
                  pesticide_sd = 0, crop_min = 0, crop_max = 1000,
                  pesticide_min = 0, pesticide_max = 1000, crop_number = 2,
                  pesticide_number = 1, print_inds = FALSE, print_gens = FALSE,
                  print_last = FALSE, K_on_birth = 1000, pesticide_start = 0,
                  immigration_rate = 0, get_f_coef = FALSE, get_stats = FALSE,
                  metabolism = 0);

alive_in_last_csv <- function(sim){
    dat <- sim[[3]];
    expect_equal(nrow(dat), 5);
    sum(dat[, "died"] == 0);
}

test_that("Leaky edge loses individuals that leave the landscape", {
    skip_on_cran();
    gmt       <- matrix(data = 0, nrow = 2, ncol = 2);
    diag(gmt) <- 1;
    mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 2, indivs = 50,
                              npsize = 50, max_gen = 2, prnt_out = FALSE);
    edge_args$mine_output <- mg;
    edge_args$land_edge   <- "leaky";
    sim_leaky <- do.call(run_farm_sim, edge_args);
    leaky_alive <- alive_in_last_csv(sim_leaky);
    expect_equal(leaky_alive, 0);
})

test_that("Torus, sticky and reflect edges keep everyone alive", {
    skip_on_cran();
    gmt       <- matrix(data = 0, nrow = 2, ncol = 2);
    diag(gmt) <- 1;
    mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 2, indivs = 50,
                              npsize = 50, max_gen = 2, prnt_out = FALSE);
    edge_args$mine_output <- mg;

    edge_args$land_edge <- "torus";
    sim_torus <- do.call(run_farm_sim, edge_args);
    expect_equal(alive_in_last_csv(sim_torus), 5);

    edge_args$land_edge <- "sticky";
    sim_sticky <- do.call(run_farm_sim, edge_args);
    expect_equal(alive_in_last_csv(sim_sticky), 5);

    # Reflect only reflects correctly for overshoots smaller than the grid
    # dimension, so use a small move_distance here.
    edge_args$land_edge     <- "reflect";
    edge_args$move_distance <- 5;
    sim_reflect <- do.call(run_farm_sim, edge_args);
    expect_equal(alive_in_last_csv(sim_reflect), 5);
})
