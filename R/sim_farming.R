#' simulate farming
#'
#' Builds a layer of the landscape with a shortest-splitline algorithm to assign
#' landscape cells among farms
#'
#'@param pests Pest array
#'@param land  Landscape array
#'@return A two dimensional array of cells with ownership values
#'@export
sim_crops <- function(pests, land){
  
  N   <- dim(pests)[1];
  W   <- dim(pests)[2];
  X   <- dim(land)[1];
  Y   <- dim(land)[2];
  Z   <- dim(land)[3];
  smu <- 1/sqrt(2); # SD of mutated loci values for sexual individuals
  amu <- 1;         # SD of mutated loci values for asexual individuals
  
  # Currently have up to 50 columns available
  paras  <- c( 0.0,   # 00) pests column for ID
               1.0,   # 01) pests column for xloc
               2.0,   # 02) pests column for yloc
               3.0,   # 03) pests column for age
               4.0,   # 04) pests column for sex
               5.0,   # 05) pests column for movement distance
               6.0,   # 06) pests column for mother ID
               7.0,   # 07) pests column for father ID
               8.0,   # 08) pests column for mother row
               9.0,   # 09) pests column for father row
              10.0,   # 10) pests column for offspring produced
              11.0,   # 11) pests column for loci number
              12.0,   # 12) pests column for trait number
              13.0,   # 13) pests column for network layers
              14.0,   # 14) pests column for food consumed
              15.0,   # 15) pests column for pesticide consumed
              16.0,   # 16) pests column for food needed survival
              17.0,   # 17) pests column for pesticide tolerated survival
              18.0,   # 18) pests column for food needed reproduction
              19.0,   # 19) pests column for pesticide tolerated reproduction
              20.0,   # 20) pests column tagging 1
              21.0,   # 21) pests column tagging 2
              22.0,   # 22) pests column tagging 3
              23.0,   # 23) pests column for reproduction type
              24.0,   # 24) pests column for mating distance
              25.0,   # 25) pests column for reproduction parameter
              26.0,   # 26) pests column for whether selfing is allowed
              27.0,   # 27) pests column for mate accessed
              28.0,   # 28) pests column for ploidy
              29.0,   # 29) pests column for neutral alleles number
              30.0,   # 30) pests column for movement bouts in a time step
              31.0,   # 31) pests column for min age of movement
              32.0,   # 32) pests column for max age of movement
              33.0,   # 33) pests column for min age of feeding
              34.0,   # 34) pests column for max age of feeding
              35.0,   # 35) pests column for min age of mating and reproduction
              36.0,   # 36) pests column for max age of mating and reproduction
              37.0,   # 37) pests column for food 1 consumption
              38.0,   # 38) pests column for food 2 consumption
              39.0,   # 39) pests column for food 3 consumption
              40.0,   # 40) pests column for food 4 consumption 
              41.0,   # 41) pests column for food 5 consumption
              42.0,   # 42) pests column for food 6 consumption
              43.0,   # 43) pests column for food 7 consumption
              44.0,   # 44) pests column for food 8 consumption
              45.0,   # 45) pests column for food 9 consumption
              46.0,   # 46) pests column for food 10 consumption
              47.0,   # 47) pests column for biopesticide 1 consumption
              48.0,   # 48) pests column for biopesticide 2 consumption
              49.0,   # 49) pests column for biopesticide 3 consumption
              50.0,   # 50) pests column for biopesticide 4 consumption
              51.0,   # 51) pests column for biopesticide 5 consumption
              52.0,   # 52) pests column for biopesticide 6 consumption
              53.0,   # 53) pests column for biopesticide 7 consumption
              54.0,   # 54) pests column for biopesticide 8 consumption
              55.0,   # 55) pests column for biopesticide 9 consumption
              56.0,   # 56) pests column for biopesticide 10 consumption
              57.0,   # 57) pests column for eating during bout
              58.0,   # 58) pests column for food 1 consumed
              59.0,   # 59) pests column for food 2 consumed
              60.0,   # 60) pests column for food 3 consumed
              61.0,   # 61) pests column for food 4 consumed
              62.0,   # 62) pests column for food 5 consumed
              63.0,   # 63) pests column for food 6 consumed
              64.0,   # 64) pests column for food 7 consumed
              65.0,   # 65) pests column for food 8 consumed
              66.0,   # 66) pests column for food 9 consumed
              67.0,   # 67) pests column for food 10 consumed
              68.0,   # 68) pests column for biopesticide 1 consumed
              69.0,   # 69) pests column for biopesticide 2 consumed
              70.0,   # 70) pests column for biopesticide 3 consumed
              71.0,   # 71) pests column for biopesticide 4 consumed
              72.0,   # 72) pests column for biopesticide 5 consumed
              73.0,   # 73) pests column for biopesticide 6 consumed
              74.0,   # 74) pests column for biopesticide 7 consumed
              75.0,   # 75) pests column for biopesticide 8 consumed
              76.0,   # 76) pests column for biopesticide 9 consumed
              77.0,   # 77) pests column for biopesticide 10 consumed
              78.0,   # 78) pests column for pesticide consumption during bout
              79.0,   # 79) pests column for mortality type
              80.0,   # 80) pests column for maximum age
              81.0,   # 81) pests column for mortality has occurred
              82.0,   # 82) pests column for age of food threshold enacted
              83.0,   # 83) pests column for age of pesticide threshold enacted
              84.0,   # 84)
              85.0,   # 85)
              86.0,   # 86)
              87.0,   # 87)
              88.0,   # 88)
              89.0,   # 89)
              90.0,   # 90)
              91.0,   # 91)
              92.0,   # 92)
              93.0,   # 93)
              94.0,   # 94)
              95.0,   # 95)
              96.0,   # 96)
              97.0,   # 97)
              98.0,   # 98)
              99.0,   # 99)
              100.0,  # 100)
              N,      # 101) Number of rows in the pest array
              0,      # 102) Torus landscape
              X,      # 103) x dimension of the landscape
              Y,      # 104) y dimension of the landscape
              Z,      # 105) z dimension (depth) of the landscape
              0,      # 106) Dynamic count of total offspring
              W,      # 107) Number of cols in the pest array
              N,      # 108) Highest ID of an individual
              100,    # 109) Column where the traits start
              0.0,    # 110) Crossover probability for sexual reproduction
              0,      # 111) Mutation type (0 = new allele; 1 = vary existing)
              0.0,    # 112) Mutation rate
              0,      # 113) Network layers that can mutate 
              0,      # 114) Mutation direction
              amu,    # 115) Mutation SD (asexual)
              smu,    # 116) Mutation SD (sexual)
              0,      # 117) Layers mutate from loci to (1) or traits back (0)
              1,      # 118) Land layer where the food 1 is located
              2,      # 119) Land layer where the food 2 is located
              3,      # 120) Land layer where the food 3 is located
              4,      # 121) Land layer where the food 4 is located
              5,      # 122) Land layer where the food 5 is located
              6,      # 123) Land layer where the food 6 is located
              7,      # 124) Land layer where the food 7 is located
              8,      # 125) Land layer where the food 8 is located
              9,      # 126) Land layer where the food 9 is located
              10,     # 127) Land layer where the food 10 is located
              11,     # 128) Land layer where the pesticide 1 is located
              12,     # 129) Land layer where the pesticide 2 is located
              13,     # 130) Land layer where the pesticide 3 is located
              14,     # 131) Land layer where the pesticide 4 is located
              15,     # 132) Land layer where the pesticide 5 is located
              16,     # 133) Land layer where the pesticide 6 is located
              17,     # 134) Land layer where the pesticide 7 is located
              18,     # 135) Land layer where the pesticide 8 is located
              19,     # 136) Land layer where the pesticide 9 is located
              20,     # 137) Land layer where the pesticide 10 is located
              N,      # 138) Number of individuals following mortality
              N       # 139) Number of individuals in the next time step
              );
  
  if(is.array(pests) == FALSE){
    stop("ERROR: pests must be a 2D array.");
  }
  if(is.array(land) == FALSE){
    stop("ERROR: land must be a 3D array.");
  }
  
  SIM_RESULTS        <- run_farming_sim(pests, land, paras);

  return(SIM_RESULTS);
}

run_farming_sim <- function(IND, LAND, PARAS){
  .Call("sim_farming", IND, LAND, PARAS);
}