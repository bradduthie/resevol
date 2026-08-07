library("testthat");
library("resevol");
context("rename_csv tests");

test_that("rename_csv renames a file without rewriting contents", {
    skip_on_cran();
    old <- tempfile(fileext = ".csv");
    new <- tempfile(fileext = ".csv");
    writeLines(c("ts,x,y", "1,2,3", "4,5,6"), old);
    expect_true(file.exists(old));
    expect_false(file.exists(new));
    resevol:::rename_csv(old, new);
    expect_false(file.exists(old));
    expect_true(file.exists(new));
    expect_equal(readLines(new), c("ts,x,y", "1,2,3", "4,5,6"));
})

test_that("rename_csv to itself is a no-op", {
    skip_on_cran();
    old <- tempfile(fileext = ".csv");
    writeLines("a,b\n1,2", old);
    resevol:::    rename_csv(old, old);
    expect_true(file.exists(old));
    expect_equal(readLines(old), c("a,b", "1,2"));
})

test_that("rename_csv errors when the source file is missing", {
    skip_on_cran();
    expect_error(resevol:::rename_csv(tempfile(fileext = ".csv"), tempfile(fileext = ".csv")),
                 "does not exist");
})

test_that("rename_csv handles a trailing-comma (no header) file", {
    skip_on_cran();
    old <- tempfile(fileext = ".csv");
    new <- tempfile(fileext = ".csv");
    writeLines(c("0,1.0,2.0,", "0,3.0,4.0,"), old);
    resevol:::rename_csv(old, new);
    expect_true(file.exists(new));
    expect_equal(readLines(new), c("0,1.0,2.0,", "0,3.0,4.0,"));
})
