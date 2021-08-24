library("testthat");
library("helicoverpa");
context("mine_gmatrix tests");

test_that("Correct number of elements in the mine_gmatrix output", {
    set.seed(Sys.time());
    gmt       <- matrix(data = 0, nrow = 2, ncol = 2);
    diag(gmt) <- 1;
    mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 2, indivs = 100, 
                              npsize = 100, max_gen = 4, prnt_out = FALSE);
    expect_equal(length(gmt), 4);
})

test_that("Correct element sizes or dimensions in the mine_gmatrix output", {
    set.seed(Sys.time());
    gmt       <- matrix(data = 0, nrow = 2, ncol = 2);
    diag(gmt) <- 1;
    mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 2, indivs = 100, 
                              npsize = 100, max_gen = 4, prnt_out = FALSE);
    expect_equal(length(mg[[1]]), 14);
    expect_equal(dim(mg[[2]]), c(2, 2));
    expect_equal(dim(mg[[3]]), c(4, 2));
    expect_equal(dim(mg[[4]]), c(2, 2, 2));
    expect_equal(dim(mg[[5]]), c(4, 2));
    expect_equal(dim(mg[[6]]), c(2, 2));
    expect_equal(length(mg[[7]]), 16);
    expect_equal(length(mg[[8]]), 1);
})