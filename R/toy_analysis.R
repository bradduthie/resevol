block_1   <- read.csv("../data/toy_results_block1b.csv");
block_10  <- read.csv("../data/toy_results_block10b.csv");
block_25  <- read.csv("../data/toy_results_block25b.csv");
block_50  <- read.csv("../data/toy_results_block50b.csv");
block_100 <- read.csv("../data/toy_results_block100b.csv");

par(mfrow = c(3, 2), mar = c(0.5, 0.5, 0.5, 0.5), oma = c(6, 6, 1, 1));
plot(x = block_1[,1],   y = block_1[,7], type = "b", xaxt = "n", 
     ylim = c(-15, 100), pch = 20, cex = 1.5, lwd = 2, cex.axis = 1.5);
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
mtext("Generation", outer = TRUE, side = 1, line = 3, cex = 1.5);
mtext("Percentage of pests surviving on landscape cell", outer = TRUE, side = 2,
      line = 3, cex = 1.5);

