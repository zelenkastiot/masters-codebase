# -----------------------------------------------
# Geometric Distribution
# ~ Models the # of tails before the first head in a sequence of coin flips.
# -----------------------------------------------

library(HDInterval) # for HDI

# Step 1: Simulate observed data from Geometric(θ)
set.seed(42)
true_theta <- 0.3
n <- 100
data <- rgeom(n, prob = true_theta) # Geometric: # of failures before first success

# Step 2: Define prior on θ
a_prior <- 2
b_prior <- 2

# Step 3: Posterior update
a_post <- a_prior + n
b_post <- b_prior + sum(data)

# Step 4: Simulate from prior and posterior
theta_prior_samples <- rbeta(10000, a_prior, b_prior)
theta_post_samples <- rbeta(10000, a_post, b_post)

# Step 5: Posterior predictive simulation
posterior_predictive <- rgeom(10000, prob = theta_post_samples)

# Step 6: MAP estimator
map_estimate <- if (a_post > 1 & b_post > 1) {
  (a_post - 1) / (a_post + b_post - 2)
} else {
  NA
}

# Step 7: 95% Credible Interval and HDI
cred_int <- quantile(theta_post_samples, c(0.025, 0.975))
hdi_int <- hdi(theta_post_samples, credMass = 0.95)

# Print results
cat("MAP Estimate of θ:", round(map_estimate, 4), "\n")
cat(
  "95% Credible Interval: [",
  round(cred_int[1], 4),
  ",",
  round(cred_int[2], 4),
  "]\n"
)
cat("95% HDI: [", round(hdi_int[1], 4), ",", round(hdi_int[2], 4), "]\n")

layout(matrix(1:4, nrow = 2, byrow = TRUE)) # 2 rows, 2 columns
par(mar = c(5, 4, 4, 6)) # Extra margin for legends outside

# 1. Prior distribution
hist(
  theta_prior_samples,
  breaks = 50,
  freq = FALSE,
  col = "lightgray",
  main = paste0("Prior: Beta(", a_prior, ", ", b_prior, ")"),
  xlab = "θ"
)

# 2. Posterior distribution with MAP, CI, and HDI
hist(
  theta_post_samples,
  breaks = 50,
  freq = FALSE,
  col = "lightblue",
  main = paste0("Posterior: Beta(", a_post, ", ", b_post, ")"),
  xlab = "θ"
)
abline(v = map_estimate, col = "red", lwd = 2, lty = 2)
abline(v = cred_int, col = "darkgreen", lty = 3)
abline(v = hdi_int, col = "purple", lty = 4, lwd = 2)
legend(
  "topright",
  inset = c(-0.3, 0),
  xpd = TRUE,
  legend = c("MAP", "95% CI", "95% HDI"),
  col = c("red", "darkgreen", "purple"),
  lty = c(2, 3, 4),
  lwd = c(2, 1, 2),
  bty = "n"
)

# 3. Posterior predictive distribution + true geometric
max_y <- max(posterior_predictive, qgeom(0.999, prob = true_theta))
breaks_vals <- seq(-0.5, max_y + 0.5, by = 1)

hist(
  posterior_predictive,
  breaks = breaks_vals,
  freq = FALSE,
  col = "orange",
  border = "white",
  main = "Posterior Predictive vs True Geometric",
  xlab = "y",
  xlim = c(0, max_y)
)

# Overlay the true Geometric PMF
y_vals <- 0:max_y
true_probs <- dgeom(y_vals, prob = true_theta)
points(y_vals, true_probs, type = "h", col = "blue", lwd = 2)
points(y_vals, true_probs, col = "blue", pch = 16)

# Legend outside the plot
legend(
  "topright",
  inset = c(-0.3, 0),
  xpd = TRUE,
  legend = c("Posterior Predictive", "True Geometric"),
  fill = c("orange", NA),
  border = c("white", NA),
  col = c("orange", "blue"),
  pch = c(NA, 16),
  lty = c(NA, 1),
  lwd = c(NA, 2),
  bty = "n"
)

# 4. Overlay prior and posterior for comparison
plot(
  density(theta_prior_samples),
  col = "gray40",
  lwd = 2,
  main = paste0(
    "Prior vs Posterior\nBeta(",
    a_prior,
    ",",
    b_prior,
    ") vs Beta(",
    a_post,
    ",",
    b_post,
    ")"
  ),
  xlab = "θ",
  ylim = c(0, 14)
)
lines(density(theta_post_samples), col = "blue", lwd = 2)
legend(
  "topright",
  legend = c("Prior", "Posterior"),
  col = c("gray40", "blue"),
  lwd = 2,
  bty = "n"
)
