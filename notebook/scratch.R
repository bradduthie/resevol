# Initialising a file
#library(helicoverpa)

 mat <- matrix(data = 0, nrow = 8, ncol = 8);
 diag(mat) <- 1;
 mg  <- mine_gmatrix(gmatrix = mat, paras = c(16, 10, 2000, 6000, 0.2, 0.01, 8000, 0.2, 200, 4, -5.3));

# Scale the variation of the initialised network to the gmatrix input.
 
# ENDORSE needs correlation matrices, not necessarily covariance matrices
 
# Write a function in mine_gmatrix.c to retain the highest fitness strategy at all times