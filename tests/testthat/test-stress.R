library("testthat");
library("resevol");
context("initialise tests");

test_that("Stress test produces a vector of expected length", {
    skip_on_cran();
    set.seed(Sys.time());
    gmt       <- matrix(data = 0, nrow = 4, ncol = 4);
    diag(gmt) <- 1;
    mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 3, indivs = 100, 
                              npsize = 100, max_gen = 2, prnt_out = FALSE);
    stresses  <- stress_test(mine_output = mg, reps = 10);
    expect_equal(length(stresses), 10);
})







