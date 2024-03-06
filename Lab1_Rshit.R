#         Rtotal     m      b      %
CAR_S1 <- c(0.121, 0.007, 320.3, 34.58)
CAR_S2 <- c(0.101, 0.010, 329.3, 4.90)
CAR_S3 <- c(0.123, 0.013, 322.3, 2.68)

UT_S1 <- c(0.063, 0.003, 333.1, 3.47)
UT_S2 <- c(0.049, 0.002, 332.6, 1.65)
UT_S3 <- c(0.044, 0.002, 333.0, 1.49)

mean_value <- mean(c(0.121,0.101,0.123,0.063,0.049,0.044))
std_dev <- sd(c(0.121,0.101,0.123,0.063,0.049,0.044))

barplot(c(0.121,0.101,0.123,0.063,0.049,0.044),
        names.arg = c("CAR_S1", "CAR_S2", "CAR_S3", "UT_S1", "UT_S2", "UT_S3"),
        col ="skyblue",
        main = "Barplot showing RTOTAL results")
abline(h = mean_value, col = "darkred", lty = 1, lwd = 2)
abline(h = mean_value - std_dev, col = "darkgreen", lty = 2)
abline(h = mean_value + std_dev, col = "darkgreen", lty = 2)

cells <- c(154, 113, 137, 86, 154, 102, 151, 70, 147, 68, 159, 59)
cells_name <- c("CAR C 1", "CAR T 1", "CAR C 2", "CAR T 2", "CAR C 3", "CAR T 3", "UT C 1", "UT T 1", "UT C 2", "UT T 2", "UT C 3", "UT T 3")

mean_value = mean(cells)
std_dev = sd(cells)

barplot(cells,
        names.arg = cells_name,
        main = "Barplot showing Cancer (C) and T cells (T) for CAR and UT using macro",
        col = "skyblue")

abline(h = mean_value, col = "darkred", lty = 1, lwd = 2)
abline(h = mean_value - std_dev, col = "darkgreen", lty = 2)
abline(h = mean_value + std_dev, col = "darkgreen", lty = 2)
