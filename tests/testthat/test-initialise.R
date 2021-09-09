library("testthat");
library("resevol");
context("initialise tests");

test_that("Correctly initialises different individual types", {
    set.seed(Sys.time());
    gmt       <- matrix(data = 0, nrow = 2, ncol = 2);
    diag(gmt) <- 1;
    mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 2, indivs = 100, 
                              npsize = 100, max_gen = 4, prnt_out = FALSE);
    inds1     <- initialise_inds(mine_output = mg, N = 40, xdim = 40, ydim = 40, 
                                 repro = "asexual", neutral_loci = 10, 
                                 max_age = 9, min_age_move = 0, 
                                 max_age_move = 9, min_age_reproduce = 0, 
                                 max_age_reproduce = 9, min_age_feed = 0, 
                                 max_age_feed = 9, food_consume = 0.25, 
                                 pesticide_consume = 0.1, rand_age = FALSE, 
                                 move_distance = 1, food_needed_surv = 0.25, 
                                 pesticide_tolerated_surv = 0.1, 
                                 food_needed_repr = 0, 
                                 pesticide_tolerated_repr = 0,
                                 reproduction_type = "lambda", 
                                 mating_distance = 1, lambda_value = 1,
                                 movement_bouts = 1, selfing = TRUE,
                                 feed_while_moving = FALSE, mortality_type = 0,
                                 age_food_threshold = NA,
                                 age_pesticide_threshold = NA);
    inds2     <- initialise_inds(mine_output = mg, N = 40, xdim = 40, ydim = 40, 
                                 repro = "sexual", neutral_loci = 10, 
                                 max_age = 9, min_age_move = 0, 
                                 max_age_move = 9, min_age_reproduce = 0, 
                                 max_age_reproduce = 9, min_age_feed = 0, 
                                 max_age_feed = 9, food_consume = 0.25, 
                                 pesticide_consume = 0.1, rand_age = FALSE, 
                                 move_distance = 1, food_needed_surv = 0.25, 
                                 pesticide_tolerated_surv = 0.1, 
                                 food_needed_repr = 0, 
                                 pesticide_tolerated_repr = 0,
                                 reproduction_type = "lambda", 
                                 mating_distance = 1, lambda_value = 1,
                                 movement_bouts = 1, selfing = TRUE,
                                 feed_while_moving = FALSE, mortality_type = 0,
                                 age_food_threshold = NA,
                                 age_pesticide_threshold = NA);
    inds3     <- initialise_inds(mine_output = mg, N = 40, xdim = 40, ydim = 40, 
                                 repro = "biparental", neutral_loci = 10, 
                                 max_age = 9, min_age_move = 0, 
                                 max_age_move = 9, min_age_reproduce = 0, 
                                 max_age_reproduce = 9, min_age_feed = 0, 
                                 max_age_feed = 9, food_consume = 0.25, 
                                 pesticide_consume = 0.1, rand_age = FALSE, 
                                 move_distance = 1, food_needed_surv = 0.25, 
                                 pesticide_tolerated_surv = 0.1, 
                                 food_needed_repr = 0, 
                                 pesticide_tolerated_repr = 0,
                                 reproduction_type = "lambda", 
                                 mating_distance = 1, lambda_value = 1,
                                 movement_bouts = 1, selfing = TRUE,
                                 feed_while_moving = FALSE, mortality_type = 0,
                                 age_food_threshold = NA,
                                 age_pesticide_threshold = NA);
    expect_equal(dim(inds1), c(40, 136));
    expect_equal(dim(inds2), c(40, 167));
    expect_equal(dim(inds3), c(40, 167));
    expect_equal(unique(inds1[,103:106]), 
                 matrix(c(111, 119, 123, 127), nrow = 1));
    expect_equal(unique(inds2[,103:107]), 
                 matrix(c(116, 124, 128, 132, 147), nrow = 1));
    expect_equal(unique(inds3[,103:107]), 
                 matrix(c(116, 124, 128, 132, 147), nrow = 1));
})







