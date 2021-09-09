library("testthat");
library("resevol");
context("landscape tests");

test_that("Landscape dimensions are correct", {
    land <- make_landscape(rows = 12, cols = 12, farms = 12);
    expect_equal(dim(land), c(12, 12, 21));
})

test_that("Number of farms is correct", {
    land     <- make_landscape(rows = 12, cols = 12, farms = 12);
    farms    <- length(unique(as.vector(land[,,1])));
    expect_equal(farms, 12);
})

test_that("Landscape farm allocation is equal", {
    land     <- make_landscape(rows = 12, cols = 12, farms = 12);
    farms    <- length(unique(as.vector(land[,,1])));
    farmland <- table(as.vector(land[,,1]));
    expect_equal(as.vector(farmland), rep(12, farms));
})