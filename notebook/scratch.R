# Initialising a file
#library(helicoverpa)

# mat <- matrix(data = 0, nrow = 8, ncol = 8);
# diag(mat) <- 1;

mat <- read.csv("notebook/gmat.csv");
gmt <- as.matrix(mat);
mg  <- mine_gmatrix(gmatrix = gmt, paras = c(18, 6, 1000, 4000, 0.2, 0.002, 20, 0.2, 400, 4, -4.61));

# Scale the variation of the initialised network to the gmatrix input.
 
# ENDORSE needs correlation matrices, not necessarily covariance matrices
 
# Write a function in mine_gmatrix.c to retain the highest fitness strategy at all times