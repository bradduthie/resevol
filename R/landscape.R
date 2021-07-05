#' Landscape initialisation
#'
#' Initialise the landscape of the G-MSE model.
#'
#'@param rows The dimension of the other side of the landscape (e.g., Longitude)
#'@param cols The dimension of one side of the landscape (e.g., Latitude)
#'@param depth The number of layers in the 3D landscape
#'@param farms The number of farms on the landscape
#'@param public_land The proportion of landscape cells that are not farmland
#'@param farm_var Does the land distribution vary among farms (>=0, <1)
#'@return the_land A cols by rows landscape with randomly distributed cell types
#'@examples
#'land <- make_landscape(rows = 10, cols = 10, depth = 2, farms = 4)
#'@export
make_landscape <- function(rows, cols, depth = 21, farms = 4,  public_land = 0, 
                           farm_var = 0){
    
    the_land  <- NULL;
    if(rows < 2){
        stop("Landscape dimensions must be 2 by 2 or greater");   
    }         
    if(cols < 2){ # Check to make sure the landcape is big enough
        stop("Landscape dimensions must be 2 by 2 or greater");   
    }
    
    who_owns       <- land_ssa(rows, cols, farms, public_land, farm_var);
    farmland       <- who_owns;
    
    extr_layers    <- rep(x = 0, times = (depth-1) * rows * cols);
    alldata        <- c(farmland, extr_layers);

    the_land       <- array(data = alldata, dim = c(rows, cols, depth));
    return(the_land);
}


#' land SSA
#'
#' Builds a layer of the landscape with a shortest-splitline algorithm to assign
#' landscape cells among farms
#'
#'@param dim_x The number of cells on the X dimension of the landscape
#'@param dim_y The number of cells on the Y dimension of the landscape
#'@param farms Number of farms among which landscape cells will be divided
#'@param public_land The amount of land that will not be owned
#'@param land_var Does distribution of land vary among users? >=0, <1
#'@return A two dimensional array of cells with ownership values
#'@export
land_ssa <- function(dim_x, dim_y, farms, public_land, land_var){
    check_val <- floor( farms / (1 - public_land) );
    if( (dim_x * dim_y) < check_val){
        warning("There are probably not enough landscape cells for users");
    }
    landscape_c_vector <- c(dim_x, dim_y, farms, public_land, land_var);
    OWNER_LAYER        <- run_landscape_a(landscape_c_vector);
    vectorise_layer    <- as.vector(OWNER_LAYER);
    return_array       <- array(data = vectorise_layer, dim = c(dim_x, dim_y));
    return(return_array);
}

run_landscape_a <- function(LANDSCAPE_PARAMETERS){
    .Call("build_ownership", LANDSCAPE_PARAMETERS);
}