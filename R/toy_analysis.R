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
plot(x = x, y = 100 - y, ylim = c(0, 100), xlab = "Monoculture block size (sq. km)", 
     ylab = "Percent of pests killed per block", type = "b", cex.lab = 1.5, 
     cex = 1.5, pch = 20, lwd = 2, cex.axis = 1.5, log = "x");



land <- toy_initialise_land(xdim = 100, ydim = 100, pathogens = 5, crops = 3);
land <- toy_block_land(xdim = 100, ydim = 100, pathogens = 5, crops = 3, 
                       block_len = 100);
par(mar = c(0, 0, 0, 0));
image(land[,,2], col = grey.colors(5), xaxt = "n", yaxt = "n");

b1   <- c(34.409, 34.017, 34.616, 34.288, 34.858, 34.878, 34.247, 34.682,
          34.822, 34.578, 34.609, 34.267, 34.233, 34.429, 34.558, 35.003,
          34.704, 34.479, 34.808, 35.486, 34.396, 34.451, 34.332, 34.637,
          34.497, 34.84,  34.939, 34.781, 34.754, 34.581, 34.511, 34.902,
          34.546, 34.174, 34.744, 34.306, 34.8,   34.362, 35.144);
b5   <- c(44.856, 46.944, 45.709, 45.443, 46.724, 46.774, 45.222, 46.865,
          47.078, 46.662, 45.724, 45.461, 46.94,  44.519, 44.572, 46.269,
          47.959, 44.943, 45.069, 46.474, 46.44,  46.864, 46.067, 47.551,
          45.878, 46.424, 49.079, 46.395, 48.002, 46.045, 46.843, 46.126);
b10  <- c(59.593, 57.933, 59.874, 60.955, 58.175, 61.865, 60.413, 57.518,
          61.982, 58.414, 59.723, 57.882, 56.756, 64.354, 59.38 , 60.335,
          56.173, 62.256, 58.949, 60.029, 62.243, 63.034, 58.813, 62.082,
          59.452)
b25  <- c(79.044, 80.991, 73.28,  80.939, 78.659, 79.751, 78.817, 76.218,
          76.033, 81.204, 76.859, 81.117, 80.114, 74.716, 79.482, 80.173,
          74.572, 79.276, 76.074, 76.084, 80.555);
b50  <- c(83.437, 89.393, 88.806, 92.516, 92.535, 82.264, 83.288, 96.704, 
          89.602, 86.701, 89.536, 82.27,  87.181, 97.347, 89.487, 82.78,
          93.039, 82.87);
b100 <- c(96.794, 96.635, 96.848, 97.607, 96.792, 96.774, 96.679, 97.577);

b1    <- 100 - b1;
b5    <- 100 - b5;
b10   <- 100 - b10;
b25   <- 100 - b25;
b50   <- 100 - b50;
b100  <- 100 - b100;

ysd  <- c(sd(b1), sd(b5), sd(b10), sd(b25), sd(b50), sd(b100));
yln  <- c(length(b1), length(b5), length(b10), length(b25), length(b50), 
          length(b100));
yy   <- c(mean(b1), mean(b5), mean(b10), mean(b25), mean(b50), mean(b100));
ymin <- c(min(b1), min(b5), min(b10), min(b25), min(b50), min(b100));
ymax <- c(max(b1), max(b5), max(b10), max(b25), max(b50), max(b100));
yl95 <- ymin; # (2.576 * ysd) / sqrt(yln);
yu95 <- ymax; # (2.576 * ysd) / sqrt(yln);
x    <- c(1, 5, 10, 25, 50, 100);




par(mar = c(5, 5, 1, 1));
plot(x = x, y = yy, ylim = c(0, 100), xlab = "Monoculture block size (sq. km)", 
     ylab = "Percent of pests killed per block", type = "b", cex.lab = 1.5, 
     cex = 1.0, pch = 20, lwd = 2, cex.axis = 1.5, log = "x");
arrows(x0 = x, x1 = x, y0 = yu95, y1 = yl95, length = 0.1, code = 3, angle = 90,
       lwd = 2);

















