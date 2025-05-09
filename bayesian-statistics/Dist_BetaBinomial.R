############################# Beta-Binomial dist

# Simulate Beta-Binomial via hierarchical sampling
set.seed(42)
theta <- rbeta(1000, 3, 11)

Y <- c()
for (i in theta) {
  r <- rbinom(100, 10, i)
  Y <- c(Y, r)
}

# Plot histogram of empirical Beta-Binomial
hist(
  Y,
  breaks = seq(-0.5, 10.5, 1),
  freq = FALSE,
  col = "skyblue",
  border = "white",
  main = expression(paste("Empirical vs. Theoretical Beta-Binomial")),
  xlab = "Number of Successes (Y)",
  ylab = "Probability/Frequency"
)

# Add theoretical Beta-Binomial PMF
x_vals <- 0:10
pmf <- dbbinom(x_vals, size = 10, alpha = 3, beta = 11)

points(x_vals, pmf, pch = 19, col = "red") # plot points
lines(x_vals, pmf, col = "red", lwd = 2) # connect them
legend(
  "topright",
  legend = c("Empirical", "Theoretical"),
  fill = c("skyblue", NA),
  border = c("white", NA),
  lty = c(NA, 1),
  pch = c(NA, 19),
  col = c("skyblue", "red")
)

# Add annotation below the legend manually
usr <- par("usr") # get current plot bounds: c(x1, x2, y1, y2)
text(
  x = usr[2] - 2.75,
  y = usr[4] - 0.055,
  labels = expression(theta %~% Beta(3, 11)),
  adj = 0
)

################### prior vs new postreior
## prior
curve(
  dbeta(x, 3, 11),
  col = "red",
  ylab = expression(p(theta)),
  xlab = expression(theta),
  ylim = c(0, 4.2)
)
## posterior
curve(dbeta(x, 9, 15), col = "green", add = TRUE)

title("prior vs. posterior")
legend(
  "topright",
  legend = c("prior: α=3, β=11", "posterior: α=9, β=15"),
  col = c("red", "green"),
  lwd = 2
)

########################################## compare
# Load required package
library(extraDistr)

# Set layout: 1 row, 2 plots
par(mfrow = c(1, 2))

# First: Beta(3, 11)
set.seed(42)
n <- 10
alpha1 <- 3
beta1 <- 11
theta1 <- rbeta(100, alpha1, beta1)
mean1 <- n * alpha1 / (alpha1 + beta1)
Y1 <- unlist(lapply(theta1, function(p) rbinom(100, n, p)))

hist(
  Y1,
  breaks = seq(-0.5, 10.5, 1),
  freq = FALSE,
  col = "skyblue",
  border = "white",
  main = expression(paste("Beta(3,11) prior")),
  xlab = "Y, Number of Successes",
  ylab = "proba/freq",
  ylim = c(0, 0.26)
)

# Overlay theoretical PMF
x_vals <- 0:10
pmf1 <- dbbinom(x_vals, size = 10, alpha = 3, beta = 11)
points(x_vals, pmf1, pch = 19, col = "red")
lines(x_vals, pmf1, col = "red", lwd = 2)

abline(v = mean1, col = "red", lty = 2, lwd = 2)
text(
  x = mean1 + 1,
  y = 0.19,
  labels = paste("Mean ≈", round(mean1, 2)),
  col = "red",
  pos = 4
)

# Second: Beta(9, 15)
alpha2 <- 9
beta2 <- 15
theta2 <- rbeta(100, alpha2, beta2)
mean2 <- n * alpha2 / (alpha2 + beta2)
Y2 <- unlist(lapply(theta2, function(p) rbinom(100, n, p)))

hist(
  Y2,
  breaks = seq(-0.5, 10.5, 1),
  freq = FALSE,
  col = "lightgreen",
  border = "white",
  main = expression(paste("Beta(9,15) prior\n. (posterior)")),
  xlab = "Y, Number of Successes",
  ylab = "proba/freq",
  ylim = c(0, 0.26)
)

# Overlay theoretical PMF
pmf2 <- dbbinom(x_vals, size = 10, alpha = 9, beta = 15)
points(x_vals, pmf2, pch = 19, col = "darkgreen")
lines(x_vals, pmf2, col = "darkgreen", lwd = 2)

abline(v = mean2, col = "darkgreen", lty = 2, lwd = 2)
text(
  x = mean2 + 1,
  y = 0.19,
  labels = paste("Mean ≈", round(mean2, 2)),
  col = "darkgreen",
  pos = 4
)
