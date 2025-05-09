# Observed data
y <- 7
n <- 10
theta_mle <- y / n

# from data
alpha <- 5
beta <- 5
map <- (alpha - 1) / (beta + alpha - 2)
# Generate samples
x <- rbeta(1000000, alpha, beta)

# Estimate density
d <- density(x)

# Find mode
mode_est <- d$x[which.max(d$y)]

# Plot histogram
hist(
  x,
  breaks = 50,
  freq = FALSE,
  col = "lightgray",
  main = "Beta(3, 11) — Histogram with Estimated Density and MAP",
  xlab = expression(theta),
  ylab = "Density, p(theta)"
)

# Overlay estimated density curve
lines(d, col = "blue", lwd = 2)

# Add vertical line for MAP (mode)
abline(v = mode_est, col = "red", lty = 2, lwd = 2)

abline(v = map, col = "darkgreen", lty = 2, lwd = 2)

# Add legend
legend(
  "topright",
  legend = c(
    "Estimated Density",
    paste("mode ≈", round(mode_est, 4)),
    paste("MAP ≈", round(map, 4))
  ),
  col = c("blue", "red", "darkgreen"),
  lty = c(1, 2),
  lwd = 2,
  bty = "n"
)
