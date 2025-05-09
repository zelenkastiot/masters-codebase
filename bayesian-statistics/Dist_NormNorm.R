# Load required libraries
library(HDInterval)

# Step 1: Simulate observed data from Normal(μ, σ²)
set.seed(42)
true_mu <- 5
sigma2 <- 4 # known variance
n <- 100
data <- rnorm(n, mean = true_mu, sd = sqrt(sigma2))

# Step 2: Prior on μ ~ Normal(μ₀, τ²)
mu0 <- 0
tau2 <- 10

# Step 3: Posterior update
posterior_variance <- 1 / (1 / tau2 + n / sigma2)
posterior_mean <- posterior_variance * (mu0 / tau2 + n * mean(data) / sigma2)

# Step 4: Simulate from prior and posterior
mu_prior_samples <- rnorm(10000, mean = mu0, sd = sqrt(tau2))
mu_post_samples <- rnorm(
  10000,
  mean = posterior_mean,
  sd = sqrt(posterior_variance)
)

# Step 5: Posterior predictive simulation
y_pred <- rnorm(10000, mean = mu_post_samples, sd = sqrt(sigma2))

# Step 6: MAP = posterior mean (Normal is symmetric)
map_estimate <- posterior_mean

# Step 7: 95% CI and HDI
cred_int <- quantile(mu_post_samples, c(0.025, 0.975))
hdi_int <- hdi(mu_post_samples, credMass = 0.95)

# Print results
cat("MAP Estimate of μ:", round(map_estimate, 4), "\n")
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
  mu_prior_samples,
  breaks = 50,
  freq = FALSE,
  col = "coral",
  main = paste0("Prior: Normal(", mu0, ", ", tau2, ")"),
  xlab = expression(mu)
)

# Annotate posterior update rule
text(
  x = mu0 + 2,
  y = max(density(mu_prior_samples)$y) * 0.8,
  labels = expression(
    atop(
      mu[n] ==
        frac(mu[0] / tau^2 + n * bar(y) / sigma^2, 1 / tau^2 + n / sigma^2),
      tau[n]^2 == frac(1, 1 / tau^2 + n / sigma^2)
    )
  ),
  adj = 0,
  cex = 0.9
)

# 2. Posterior distribution with MAP, CI, and HDI
hist(
  mu_post_samples,
  breaks = 50,
  freq = FALSE,
  col = "lightblue",
  main = "Posterior Distribution",
  xlab = expression(mu)
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

# 3. Posterior predictive vs true Normal
hist(
  y_pred,
  breaks = 50,
  freq = FALSE,
  col = "orange",
  border = "white",
  main = "Posterior Predictive vs True Normal",
  xlab = "y",
  xlim = range(y_pred)
)

x_vals <- seq(min(y_pred), max(y_pred), length.out = 300)
true_density <- dnorm(x_vals, mean = true_mu, sd = sqrt(sigma2))
lines(x_vals, true_density, col = "blue", lwd = 2)
legend(
  "topright",
  inset = c(-0.3, 0),
  xpd = TRUE,
  legend = c(
    "Posterior Predictive",
    bquote("N(" ~ mu == .(true_mu) ~ ", " ~ sigma^2 == .(sigma2) ~ ")")
  ),
  fill = c("orange", NA),
  border = c("white", NA),
  col = c("orange", "blue"),
  pch = c(NA, 16),
  lty = c(NA, 1),
  lwd = c(NA, 2),
  bty = "n"
)

# 4. Overlay prior and posterior
plot(
  density(mu_prior_samples),
  col = "gray40",
  lwd = 2,
  main = "Prior vs Posterior",
  xlab = expression(mu),
  ylim = c(0, max(density(mu_post_samples)$y))
)
lines(density(mu_post_samples), col = "blue", lwd = 2)
legend(
  "topright",
  legend = c("Prior", "Posterior"),
  col = c("gray40", "blue"),
  lwd = 2,
  bty = "n"
)
