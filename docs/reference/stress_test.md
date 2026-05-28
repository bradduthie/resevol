# Mine_gmatrix output stress

Run a diagnoistic test on the stress output of mine_gmatrix. The
mine_gmatrix function produces a network from loci to traits for a
pre-specified trait covariance structure. This covariance structure is
estimated from network values, but can vary due to error in the loci
values (standard random normal numbers). This function will test the
stress of a network (mean deviation between the estimated covariance
matrix and the pre-specified one) a given number of times to produce a
distribution of stress estimates.

## Usage

``` r
stress_test(mine_output, indivs = 1000, reps = 10)
```

## Arguments

- mine_output:

  The output from mine_gmatrix, which will be used to initialise the
  genomes and traits of pests.

- indivs:

  The number of individuals to use in the stress test. Higher values
  produce a larger sample size for more accurate estimates of the true
  covariance structure produced by the network, and therefore the actual
  stress expected from it.

- reps:

  The number of times a new set of individuals will be initialised for
  estimating the covariance between traits and calculating its stress.

## Value

A vector of stress values the same length as 'reps'.

## Examples

``` r
gmt       <- matrix(data = 0, nrow = 4, ncol = 4);
diag(gmt) <- 1;
mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 3, indivs = 100, 
                         npsize = 100, max_gen = 2, prnt_out = FALSE);
stresses  <- stress_test(mine_output = mg);
```
