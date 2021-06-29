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
  
  N <- dim(pests)[1];
  W <- dim(pests)[2];
  X <- dim(land)[1];
  Y <- dim(land)[2];
  Z <- dim(land)[3];
  
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
              15.0,   # 15) pests column for pesticide consumption
              16.0,   # 16) pests column for food needed survival
              17.0,   # 17) pests column for pesticide tolerated survival
              18.0,   # 18) pests column for food needed reproduction
              19.0,   # 19) pests column for pesticide tolerated survival
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
              30.0,   # 30)
              31.0,   # 31)
              32.0,   # 32)
              33.0,   # 33)
              34.0,   # 34)
              35.0,   # 35)
              36.0,   # 36)
              37.0,   # 37)
              38.0,   # 38)
              39.0,   # 39)
              40.0,   # 40)
              41.0,   # 41)
              42.0,   # 42)
              43.0,   # 43)
              44.0,   # 44)
              45.0,   # 45)
              46.0,   # 46)
              47.0,   # 47)
              48.0,   # 48)
              49.0,   # 49)
              50.0,   # 50)
              N,      # 51) Number of rows in the pest array
              0,      # 52) Torus landscape
              X,      # 53) x dimension of the landscape
              Y,      # 54) y dimension of the landscape
              Z,      # 55) z dimension (depth) of the landscape
              0,      # 56) Dynamic count of total offspring
              W,      # 57) Number of cols in the pest array
              N,      # 58) Highest ID of an individual
              50,     # 59) Column where the traits start
              0.0     # 60) Crossover probability for sexual reproduction
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