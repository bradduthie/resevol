# Add column names to individuals output

Add column names to the individual-level output CSV files made by the
run_farm_sim function. These files are printed out by run_farm_sim
whenever 'print_inds = TRUE' or 'print_last = TRUE', which cause CSV
files to be printed to 'individuals.csv' and 'last_time_step.csv',
respectively. The latter filename can be customised to something else
with the last_step_filename argument in run_farm_sim. These are large
CSV files, and because of the way simulations must be set up to allow
for varying genome sizes and trait numbers, column number is not always
the same This function individuals_colnames adds column names to a CSV
file of individuals output from 'run_farm_sim'.

## Usage

``` r
individuals_colnames(last_step_filename, mine_output)
```

## Arguments

- last_step_filename:

  Output of individuals from run_farm_sim function

- mine_output:

  The output from mine_gmatrix, which will be used to initialise the
  genomes and traits of pests.

## Value

Returns a success message and overwrites CSV file with headers

## Examples

``` r
gmt       <- matrix(data = 0, nrow = 4, ncol = 4);
diag(gmt) <- 1;
mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 3, indivs = 100, 
                         npsize = 100, max_gen = 2, prnt_out = FALSE);
individuals_colnames("last_time_step.csv", gmt);
#> [1] "No file named last_time_step.csv"
```
