# Load required libraries
library(HDInterval)
library(extraDistr)

# Step 1: Simulate observed data from Binomial(n_trials, theta)
set.seed(42)
true_theta <- 0.6 # True probability of success
n_trials <- 10 # Number of trials per experiment
n_obs <- 100 # Number of observations
data <- rbinom(n_obs, size = n_trials, prob = true_theta)
y_sum <- sum(data) # Total number of successes

# Step 2: Define prior on θ ~ Beta(α, β)
alpha_prior <- 2
beta_prior <- 2

# Step 3: Posterior update
alpha_post <- alpha_prior + y_sum
beta_post <- beta_prior + n_obs * n_trials - y_sum

# Step 4: Simulate from prior and posterior
theta_prior_samples <- rbeta(10000, alpha_prior, beta_prior)
theta_post_samples <- rbeta(10000, alpha_post, beta_post)

# Step 5: Posterior predictive simulation
y_pred <- rbinom(10000, size = n_trials, prob = theta_post_samples)

# Step 6: MAP estimate
map_estimate <- if (alpha_post > 1 && beta_post > 1) {
  (alpha_post - 1) / (alpha_post + beta_post - 2)
} else {
  NA
}

# Step 7: Credible Interval and HDI
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

# Step 8: Layout and plots
layout(matrix(1:4, nrow = 2, byrow = TRUE))
par(mar = c(5, 4, 4, 6))

# 1. Prior distribution
hist(
  theta_prior_samples,
  breaks = 50,
  freq = FALSE,
  col = "lightgray",
  main = paste0("Prior: Beta(", alpha_prior, ", ", beta_prior, ")"),
  xlab = expression(theta)
)

# 2. Posterior distribution with MAP, CI, and HDI
hist(
  theta_post_samples,
  breaks = 50,
  freq = FALSE,
  col = "lightblue",
  main = paste0("Posterior: Beta(", alpha_post, ", ", beta_post, ")"),
  xlab = expression(theta)
)
abline(v = map_estimate, col = "red", lwd = 2, lty = 2)
abline(v = cred_int, col = "darkgreen", lty = 3)
abline(v = hdi_int, col = "purple", lwd = 2, lty = 4)
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

# 3. Posterior predictive vs true binomial
hist(
  y_pred,
  breaks = seq(-0.5, n_trials + 0.5, by = 1),
  freq = FALSE,
  col = "orange",
  main = "Posterior Predictive vs True Binomial",
  xlab = "y",
  xlim = c(-0.5, n_trials + 0.5)
)
points(
  0:n_trials,
  dbinom(0:n_trials, size = n_trials, prob = true_theta),
  type = "b",
  col = "blue",
  pch = 16,
  lwd = 2
)
legend(
  "topright",
  inset = c(-0.3, 0),
  xpd = TRUE,
  legend = c(
    "Posterior Predictive",
    bquote("Binom(" ~ theta == .(true_theta) ~ ")")
  ),
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
  main = "Prior vs Posterior",
  xlab = expression(theta),
  ylim = c(0, max(density(theta_post_samples)$y))
)
lines(density(theta_post_samples), col = "blue", lwd = 2)
legend(
  "topright",
  legend = c("Prior", "Posterior"),
  col = c("gray40", "blue"),
  lwd = 2,
  bty = "n"
)
