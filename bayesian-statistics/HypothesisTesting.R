# Null hypothesis
# 𝐻0 --- The coin is fair → θ=0.5

# Alternative hypothesis
# 𝐻1 --- The coin is not fair → 𝜃 ≠ 0.5

library(HDInterval)

# -------------------------------------------
# Step 1: Simulate observed data
# -------------------------------------------
set.seed(123)
n <- 100 # Number of coin flips
theta_true <- 0.65 # True bias (for simulation)
y <- rbinom(1, n, theta_true) # Simulate # of heads

# -------------------------------------------
# Step 2: Define prior on theta ~ Beta(1, 1)
# This is a uniform (non-informative) prior; no bias
# -------------------------------------------
a_prior <- 1
b_prior <- 1

# -------------------------------------------
# Step 3: Posterior update with observed data
# Beta posterior parameters: prior + data
# -------------------------------------------
a_post <- a_prior + y
b_post <- b_prior + n - y

# -------------------------------------------
# Step 4: Compute prior and posterior density at null value theta = 0.5
# This is the core of the Bayes Factor calculation (Savage-Dickey method)
# -------------------------------------------
theta_0 <- 0.5
prior_density <- dbeta(theta_0, a_prior, b_prior) # = 1 (flat prior)
posterior_density <- dbeta(theta_0, a_post, b_post) # posterior density at null

# -------------------------------------------
# Step 5: Compute Bayes Factor
# BF_01 = evidence for null over alternative (higher = supports null)
# BF_10 = inverse (supports alternative)
# -------------------------------------------
BF_01 <- prior_density / posterior_density
BF_10 <- 1 / BF_01

# -------------------------------------------
# Step 6: Posterior samples and 95% HDI
# -------------------------------------------
theta_samples <- rbeta(10000, a_post, b_post)
hdi_int <- hdi(theta_samples, credMass = 0.95)

# -------------------------------------------
# Step 7: Report results
# -------------------------------------------
cat("Observed heads:", y, "out of", n, "\n")
cat("Posterior mean of θ:", round(mean(theta_samples), 4), "\n")
cat("95% HDI: [", round(hdi_int[1], 4), ",", round(hdi_int[2], 4), "]\n")
cat("Prior density at θ = 0.5:", round(prior_density, 4), "\n")
cat("Posterior density at θ = 0.5:", round(posterior_density, 4), "\n")
cat("Bayes Factor BF_01 (H0 vs H1):", round(BF_01, 4), "\n")
cat("Bayes Factor BF_10 (H1 vs H0):", round(BF_10, 4), "\n")

# -------------------------------------------
# Interpretation (based on your output):
# - You observed 65 heads out of 100 → more than expected under a fair coin.
# - Posterior mean is ~0.647, and 95% HDI = [0.5552, 0.7393]
# - θ = 0.5 is outside the HDI → fair coin not credible
# - Posterior density at θ = 0.5 is much lower than prior (0.0872 vs 1)
# - Bayes Factor BF_01 = 11.46 → the data made the null 11.5x less plausible
# - So, BF_10 = 0.087 → strong Bayesian evidence for the alternative (biased coin)
# -------------------------------------------

# -------------------------------------------
# Step 8: Plotting
# Left: posterior distribution of theta with null line at 0.5
# Right: overlay of prior and posterior, showing shift away from null
# -------------------------------------------
layout(matrix(1:2, nrow = 1))

# Plot 1: Posterior distribution
hist(
  theta_samples,
  breaks = 50,
  freq = FALSE,
  col = "lightblue",
  main = expression("Posterior Distribution of " * theta),
  xlab = expression(theta),
  xlim = c(0, 1)
)
abline(v = 0.5, col = "red", lwd = 2, lty = 2)
legend(
  "topleft",
  legend = c(expression(theta == 0.5 ~ "(null)")),
  col = "red",
  lty = 2,
  lwd = 2,
  bty = "n"
)

# Plot 2: Prior vs Posterior overlay
curve(
  dbeta(x, a_prior, b_prior),
  col = "gray40",
  lwd = 2,
  xlim = c(0, 1),
  ylim = c(0, 10),
  ylab = "Density",
  xlab = expression(theta),
  main = "Prior vs Posterior"
)
curve(dbeta(x, a_post, b_post), col = "blue", lwd = 2, add = TRUE)
abline(v = 0.5, col = "red", lty = 2, lwd = 2)
legend(
  "topleft",
  legend = c("Prior", "Posterior", expression(theta == 0.5)),
  col = c("gray40", "blue", "red"),
  lty = c(1, 1, 2),
  lwd = 2,
  bty = "n"
)
