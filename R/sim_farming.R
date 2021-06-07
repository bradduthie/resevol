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
  
  paras              <- c(0.0, 1.0, 2.0, 2.1); # Location of ID, xloc, and yloc
  
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