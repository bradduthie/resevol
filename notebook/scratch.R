# Initialising a file
#library(helicoverpa)

gmt <- matrix(data = 0, nrow = 4, ncol = 4);
diag(gmt) <- 1;
mg  <- mine_gmatrix(gmatrix = gmt, loci = 8, indivs = 1000, npsize = 8000, 
                    max_gen = 100, sampleK = 400, chooseK = 4);

pests <- initialise_inds(mine_output = mg);
write.csv(pests, "notebook/pests.csv", row.names = FALSE);


pests <- read.csv("notebook/pests.csv");
land  <- make_landscape(rows = 100, cols = 100, depth = 2);

sim_crops(pests, land);








# mat <- matrix(data = 0, nrow = 8, ncol = 8);
# diag(mat) <- 1;

mat <- read.csv("notebook/gmat.csv");
gmt <- as.matrix(mat);
mg  <- mine_gmatrix(gmatrix = gmt, indivs  = 10000, npsize = 12000, 
                    max_gen = 2000, sampleK = 3200, chooseK = 40, term_cri = -7, 
                    use_cor = TRUE);


#==============================================================================#
#   IMPORTANT THINGS TO DO WITH THE mine_gmatrix FUNCTION
#==============================================================================#
# Scale the variation of the initialised network to the gmatrix input.
# Either that, or allow the variation to be set in the R function
# Maybe set it to the digit just below the highest (e.g., 1 then 0.1, 10 then 1)
#
# Have an option for finding the correlation matrix rather that VCV
# Figure out how to scale the trait values appropriately
#
# Have an option for diploid genetic architecture
#==============================================================================#



N      <- 100000;
loci   <- 18;
inds   <- matrix(data = rnorm(n = (N*loci), sd = 1), nrow = N, ncol = loci);
trts   <- inds %*% mg[[5]];
cov(trts);

# Gen: 11999	 Stress: -1.809979	 Min: -4.980156

vcv <- mg[[2]];
cvs <- cor(trts);
N   <- (9 * 9) - (0.5*(9)*(9-1));
val <- 0;
for(i in 1:9){
    for(j in 1:9){
        if(j <= i){
            val <- val + ((vcv[i,j] - cvs[i,j]) * (vcv[i,j] - cvs[i,j]) / N);
        }
    }
}


mg[[3]] %*% mg[[4]][,,1] %*% mg[[4]][,,2] %*% mg[[4]][,,3];



mat <- trts;

mns <- rep(0, dim(mat)[2]);
for(i in 1:dim(mat)[2]){
    mns[i] <- 0;
    for(j in 1:dim(mat)[1]){
        mns[i] <- mat[j,i];
    }
    mns[i] <- mns[i] / dim(mat)[1];
}

VCV <- matrix(data = 0, nrow = dim(mat)[2], ncol = dim(mat)[2]);
for(i in 1:dim(mat)[2]){
    for(j in 1:dim(mat)[2]){
         VCV[i, j] <- 0;
         VCV[j, i] <- 0;
         for(k in 1:dim(mat)[1]){
             VCV[i,j] <- VCV[i, j] + (mat[k,i] - mns[i]) * (mat[k,j] - mns[j]);
         }
         VCV[i, j] <- VCV[i, j] / (dim(mat)[1] - 1);
         VCV[j, i] <- VCV[i, j];
    }
}




