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