#' Landscape initialisation
#'
#' Initialise the landscape for a simulation. This should not normally need to 
#' be done explicitly with this function because it is run inside of the 
#' run_farm_sim function, but this gives the option to generate a landscape 
#' without actually running a simulation. All landscapes are produced as three
#' dimensional arrays with varying numbers of rows and columns that determine
#' landscape size, and a depth of 21 layers. The top layer defines which cells
#' belong to which farm, while the remaining layers define how much of a given
#' crop is on the landscape cell (2-11) or how much pesticide has been applied
#' to it (12-21). An arbitrary number of farms are placed in a blocked design 
#' on the landscape using a shortest split-line algorithm, which attempts to 
#' make farm size as even as possible. Specifying public land is possible, and
#' adds sections of land that are not farms, but this is not recommended.
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
#'@useDynLib resevol
#'@importFrom stats rnorm rpois runif
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
    farmland       <- who_owns - 1;
    
    extr_layers    <- rep(x = 0, times = (depth-1) * rows * cols);
    alldata        <- c(farmland, extr_layers);

    the_land       <- array(data = alldata, dim = c(rows, cols, depth));
    return(the_land);
}


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

crop_transitions <- function(rotation_type = 1, crop_number){
    crop_N      <- crop_number;
    custom_land <- is.matrix(rotation_type);
    if(custom_land == TRUE){
        check_dims  <- dim(rotation_type);
        if(check_dims[1] != check_dims[2]){
            stop("ERROR: Transition matrices must be square.")
        }
        if(check_dims[1] != crop_N){
            stop("ERROR: Transition matrix dimensions must equal crop numbers");
        }
        check_probs <- apply(X = rotation_type, MARGIN = 1, FUN = sum);
        bad_probs   <- sum(check_probs != 1);
        if(bad_probs > 0){
            stop("ERROR: Row values of transition matrices must sum to 1.")
        }
        tmat <- rotation_type;
    }else{
        tmat <- NA;
        if(rotation_type == 1){
            tmat       <- matrix(data = 0, nrow = crop_N, ncol = crop_N);
            diag(tmat) <- 1;
        }
        if(rotation_type == 2){
            tmat <- matrix(data = 1/crop_N, nrow = crop_N, ncol = crop_N);
        }
        if(rotation_type == 3){
            tmat <- matrix(data = 0, nrow = crop_N, ncol = crop_N);
            for(i in 1:crop_N){
                if(i < crop_N){
                    tmat[i, i + 1] <- 1;
                }else{
                    tmat[i, 1]     <- 1;
                }
            }
        }
        if(is.na(tmat[1]) == TRUE){
            stop("ERROR: Could not build the transition matrix.")
        }
    }
    return(tmat);
}

pesticide_transitions <- function(rotation_type = 1, pesticide_number){
    pest_N      <- pesticide_number;
    custom_land <- is.matrix(rotation_type);
    if(custom_land == TRUE){
        check_dims  <- dim(rotation_type);
        if(check_dims[1] != check_dims[2]){
            stop("ERROR: Transition matrices must be square.")
        }
        if(check_dims[1] != pest_N){
            stop("ERROR: Transition matrix dimensions must equal crop numbers");
        }
        check_probs <- apply(X = rotation_type, MARGIN = 1, FUN = sum);
        bad_probs   <- sum(check_probs != 1);
        if(bad_probs > 0){
            stop("ERROR: Row values of transition matrices must sum to 1.")
        }
        tmat <- rotation_type;
    }else{
        tmat <- NA;
        if(rotation_type == 1){
            tmat       <- matrix(data = 0, nrow = pest_N, ncol = pest_N);
            diag(tmat) <- 1;
        }
        if(rotation_type == 2){
            tmat <- matrix(data = 1/pest_N, nrow = pest_N, ncol = pest_N);
        }
        if(rotation_type == 3){
            tmat <- matrix(data = 0, nrow = pest_N, ncol = pest_N);
            for(i in 1:pest_N){
                if(i < pest_N){
                    tmat[i, i + 1] <- 1;
                }else{
                    tmat[i, 1]     <- 1;
                }
            }
        }
        if(is.na(tmat[1]) == TRUE){
            stop("ERROR: Could not build the transition matrix.")
        }
    }
    return(tmat);
}

initialise_crops <- function(crop_init = "random", crop_N, farms){
    init_mat <- matrix(data = 0, nrow = farms, ncol = crop_N);
    if(crop_init[1] == "random"){
        choice <- sample(x = 1:crop_N, size = farms, replace = TRUE);
        for(i in 1:farms){
            init_mat[i, choice[i]] <- 1;
        }
    }else{
        choice <- crop_init;
        if(length(choice) != farms){
            stop("ERROR: Initialised crop choices must equal farm number");
        }
        poss_crops <- 1:crop_N;
        if(sum(choice %in% poss_crops == FALSE) > 0){
            stop("ERROR: choices must be value from 1 to crop number");
        }
        for(i in 1:farms){
            init_mat[i, choice[i]] <- 1;
        }
    }
    return(init_mat);
}

initialise_pesticide <- function(pesticide_init = "random", pesticide_N, farms){
    init_mat <- matrix(data = 0, nrow = farms, ncol = pesticide_N);
    if(pesticide_init == "random"){
        choice <- sample(x = 1:pesticide_N, size = farms, replace = TRUE);
        for(i in 1:farms){
            init_mat[i, choice[i]] <- 1;
        }
    }else{
        choice <- pesticide_init;
        if(length(choice) != farms){
            stop("ERROR: Initialised pesticide choices must equal farm number");
        }
        poss_crops <- 1:pesticide_N;
        if(sum(choice %in% poss_crops == FALSE) > 0){
            stop("ERROR: choices must be value from 1 to pesticide number");
        }
        for(i in 1:farms){
            init_mat[i, choice[i]] <- 1;
        }
    }
    return(init_mat);
}


