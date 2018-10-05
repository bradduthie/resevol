block_1   <- read.csv("../data/block_len_1.csv");
block_10  <- read.csv("../data/block_len_10.csv");
block_25  <- read.csv("../data/block_len_25.csv");
block_50  <- read.csv("../data/block_len_50.csv");
block_100 <- read.csv("../data/block_len_100.csv");
block_200 <- read.csv("../data/block_len_200.csv");

par(mfrow = c(3, 2), mar = c(0.5, 0.5, 0.5, 0.5), oma = c(6, 6, 1, 1));
plot(x = block_1[,1],   y = block_1[,7], type = "b", xaxt = "n", 
     xlim = c(1, 40), ylim = c(-15, 100), pch = 20, cex = 1.5, lwd = 2, 
     cex.axis = 1.5);
abline(h = 0, lty = "dotted", lwd = 0.8);
text(x = 10, y = -10, labels = "1 cell blocks", cex = 1.5);
plot(x = block_10[,1],  y = block_10[,7], type = "b", xaxt = "n", yaxt = "n",
     ylim = c(-15, 100), pch = 20, cex = 1.5, lwd = 2);
abline(h = 0, lty = "dotted", lwd = 0.8);
text(x = 10, y = -10, labels = "10 cell blocks", cex = 1.5);
plot(x = block_25[,1],  y = block_25[,7], type = "b", xaxt = "n", 
     ylim = c(-15, 100), pch = 20, cex = 1.5, lwd = 2, cex.axis = 1.5);
abline(h = 0, lty = "dotted", lwd = 0.8);
text(x = 10, y = -10, labels = "25 cell blocks", cex = 1.5);
plot(x = block_50[,1],  y = block_50[,7], type = "b", xaxt = "n", yaxt = "n",
     ylim = c(-15, 100), pch = 20, cex = 1.5, lwd = 2);
abline(h = 0, lty = "dotted", lwd = 0.8);
text(x = 10, y = -10, labels = "50 cell blocks", cex = 1.5);
plot(x = block_100[,1], y = block_100[,7], type = "b", 
     ylim = c(-15, 100), pch = 20, cex = 1.5, lwd = 2, cex.axis = 1.5);
abline(h = 0, lty = "dotted", lwd = 0.8);
text(x = 10, y = -10, labels = "100 cell blocks", cex = 1.5);
plot(x = block_200[,1],  y = block_200[,7], type = "b", xaxt = "n", yaxt = "n",
     ylim = c(-15, 100), pch = 20, cex = 1.5, lwd = 2);
abline(h = 0, lty = "dotted", lwd = 0.8);
text(x = 10, y = -10, labels = "200 cell blocks", cex = 1.5);
mtext("Generation", outer = TRUE, side = 1, line = 3, cex = 1.5);
mtext("Percentage of pests surviving on landscape cell", outer = TRUE, side = 2,
      line = 3, cex = 1.5);




x <- c(1, 5, 10, 25, 50, 100);
y <- c(32.604, 43.496, 54.745, 74.626, 77.373, 88.182);

par(mar = c(5, 5, 1, 1));
plot(x = x, y = y, ylim = c(0, 100), xlab = "Heterogeny block size (sq. km)", 
     ylab = "Percent of pests surviving per block", type = "b", cex.lab = 1.5, 
     cex = 1.5, pch = 20, lwd = 2, cex.axis = 1.5);



land <- toy_initialise_land(xdim = 100, ydim = 100, pathogens = 5, crops = 3);
land <- toy_block_land(xdim = 100, ydim = 100, pathogens = 5, crops = 3, 
                       block_len = 50);
par(mar = c(0, 0, 0, 0));
image(land[,,2], col = grey.colors(5), xaxt = "n", yaxt = "n");