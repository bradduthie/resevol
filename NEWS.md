# resevol 0.4.1.0

## NEW FEATURES

* New `initial_inds` argument for `run_farm_sim` to continue a simulation from the
  final individuals of a previous run. `initial_inds` can be a matrix or data frame
* `run_farm_sim` now returns a list of three elements; the third element is a matrix
  of all individuals and their traits in the final time step, with column names
  attached, which can be passed directly to `initial_inds` in a new `run_farm_sim`
  call
* When individuals are passed with `initial_inds`, generation time and individual
  IDs continue from the previous simulation instead of resetting, so individuals
  remain uniquely identifiable across chained runs

## MAJOR CHANGES

* Population buffers are now swapped between time steps (double buffering) instead of
  freed and re-allocated each step, greatly reducing memory churn for large
  simulations 
* `mine_gmatrix` now brackets all random number draws with `GetRNGstate` /
  `PutRNGstate`, fixing a hang and degenerate random numbers when `mine_gmatrix` was
  the first random number use in a fresh R session

## BUG FIXES

* `rename_csv` now renames output files directly instead of reading and re-writing
  them, fixing intermittent corruption of large individual output files 
* Edge effects no longer fall through to the toroidal wrap: leaky, reflective, and
  sticky edges behaved like wrapping edges; leaky edges now correctly remove
  individuals from the landscape 
* Population buffers are now tracked by size and freed exactly once on all simulation
  paths, fixing a memory leak and reads past the end of buffers when populations went
  extinct
* Failed memory allocations now raise a clean R error instead of crashing
* `mine_gmatrix` now returns the lowest-stress network found during the evolutionary
  search instead of a random tournament winner 
* The mutation rate in the `mine_gmatrix` evolutionary search is now correctly
  applied; previously every network element was mutated every generation
* Fixed an error in the `individuals_colnames` example

## OTHER

* Added tests for file renaming, edge effects, extinction, and passing individuals
  between runs


# resevol 0.4.0.5

## BUG FIXES

* Validation of rotation type and rotation time now only applies when `rotation_type`
  is not a custom matrix, so custom crop and pesticide rotation matrices work as
  intended


# resevol 0.4.0.4

## DOCUMENTATION

* Added a reference for the evolutionary algorithm (Luke 2013) and other
  documentation updates (issues 55, 56)


# resevol 0.4.0.3

## BUG FIXES

* Crop and pesticide rotation type is no longer required to be 1, 2, or 3 when a
  custom rotation matrix is supplied


# resevol 0.4.0.2

## DOCUMENTATION

* Updated package references and fixed a DOI issue reported on win-builder

# resevol 0.4.0.0

## NEW FEATURES

* New function to add column names to individuals output CSV
* Custom file names are allowed for output files
* New argument `pesticide_threshold` sets density threshold for pesticide
* New argument `pesticide_threshold_delay` delays pesticide threshold

## MAJOR CHANGES

* Default neutral allele number is now 10 instead of 1000

## BUG FIXES

* Fixed issue causing an error for scalar `food_consume` or `pesticide_consume`
* Fixed a bug that was causing farmers to not apply pesticide


# resevol 0.3.4.0

## NEW FEATURES

* New argument to model crop growth over time within a season

# resevol 0.3.3.0

## NEW FEATURES

* Vignettes included demonstrating advanced options for simulations

# resevol 0.3.2.0

## NEW FEATURES

* New argument allowing users to set initialised means for evolving traits

# resevol 0.3.1.0

## MAJOR CHANGES

* Evolutionary algorithm termination criteria now based on mean stress

## NEW FEATURES

* New function to evaluate the stress of mined gmatrices

# resevol 0.3.0.1

## NEW FEATURES

* Custom landscapes allowing flexible structure to land types
* More flexible rotation of pesticides and crops on landscapes

# resevol VERSION 0.2.0.9

## BUG FIXES

* Fixed a memory issue causing a CRAN ERROR on Windows ix86 