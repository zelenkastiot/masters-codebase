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
