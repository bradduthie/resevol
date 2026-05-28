# Initialise individuals

Initialise new individuals into the IBM. This function is generally not
needed because it is run inside the run_farm_sim function to generate
new individuals for simulations. To initialise individuals with this
function, it is necessary to set the mine_output argument to output from
the mine_gmatrix function. This output includes all of the information
necessary to build individuals with genomes that produce traits that
covary in a pre-specified way. The arguments of this function include
addition information for building the individual array, which is a
two-dimensional array in which each individual occupies a row, and each
column specifies a character of the individual (including all genome
loci). See vignettes for a more detailed explanation.

## Usage

``` r
initialise_inds(
  mine_output,
  N = 1000,
  xdim = 100,
  ydim = 100,
  repro = "sexual",
  neutral_loci = 10,
  max_age = 9,
  min_age_move = 0,
  max_age_move = 9,
  min_age_reproduce = 0,
  max_age_reproduce = 9,
  min_age_feed = 0,
  max_age_feed = 9,
  food_consume = 0.25,
  pesticide_consume = 0.1,
  rand_age = FALSE,
  move_distance = 1,
  food_needed_surv = 0.25,
  pesticide_tolerated_surv = 0.1,
  food_needed_repr = 0,
  pesticide_tolerated_repr = 0,
  reproduction_type = "lambda",
  mating_distance = 1,
  lambda_value = 1,
  movement_bouts = 1,
  selfing = TRUE,
  feed_while_moving = FALSE,
  pesticide_while_moving = FALSE,
  mortality_type = 0,
  age_food_threshold = NA,
  age_pesticide_threshold = NA,
  metabolism = 0,
  baseline_metabolism = 0,
  min_age_metabolism = 1,
  max_age_metabolism = 9,
  trait_means = NULL
)
```

## Arguments

- mine_output:

  The output from mine_gmatrix

- N:

  Number of individuals to be initialised

- xdim:

  Horizontal dimensions of the landscape

- ydim:

  Vertical dimensions of the landscape

- repro:

  Type of reproduction allowed: "asexual", "sexual", and "biparental".
  Note that if repro != "asexual", this causes a diploid genome.

- neutral_loci:

  The number of neutral loci individuals have (must be \> 0)

- max_age:

  The maximum age of an individual

- min_age_move:

  The minimum age at which an individual can move

- max_age_move:

  The maximum age at which an individual can move

- min_age_reproduce:

  The minimum age which an individual can reproduce

- max_age_reproduce:

  The maximum age which an individual can reproduce

- min_age_feed:

  The minimum age at which an individual feeds

- max_age_feed:

  The maximum age at which an individual feeds

- food_consume:

  The amount of food consumed during feeding

- pesticide_consume:

  Amount of pesticide consumed while on a cell

- rand_age:

  Initialise individuals with a random age (TRUE/FALSE)

- move_distance:

  Maximum cells moved in one bout of movement

- food_needed_surv:

  Food needed to survive (if over min_age_feed)

- pesticide_tolerated_surv:

  Pesticide tolerated by individual

- food_needed_repr:

  Food needed to reproduce 1 offspring

- pesticide_tolerated_repr:

  Pesticide tolerated to allow reproduction

- reproduction_type:

  Poisson reproduction ("lambda") vs "food_based"

- mating_distance:

  Distance in cells within which mate is available

- lambda_value:

  individual value for poisson reproduction

- movement_bouts:

  Number of bouts of movement per time step

- selfing:

  If sexual reproduction, is selfing allowed? (TRUE/FALSE)

- feed_while_moving:

  Do individuals feed after each movement bout?

- pesticide_while_moving:

  Individuals consume pesticide after move bout?

- mortality_type:

  Type of mortality (currently only one option)

- age_food_threshold:

  Age at which food threshold is enacted

- age_pesticide_threshold:

  Age at which pesticide threshold is enacted

- metabolism:

  The amount of consumed food lost each time step

- baseline_metabolism:

  A fixed baseline rate added to 'metabolism'+

- min_age_metabolism:

  The minimum age affected by metabolism

- max_age_metabolism:

  The maximum age affected by metabolism

- trait_means:

  The means of the custom pest traits

## Value

A two-dimensional array of individuals for simulation

## Examples

``` r
gmt       <- matrix(data = 0, nrow = 2, ncol = 2);
diag(gmt) <- 1;
mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 2, indivs = 100, 
                         npsize = 100, max_gen = 4, prnt_out = FALSE);
inds      <- initialise_inds(mine_output = mg, N = 40, repro = "asexual");
```
