# Load required libraries
library(HDInterval)

# Step 1: Simulate observed data from Poisson(λ)
set.seed(42)
true_lambda <- 4
n <- 100
data <- rpois(n, lambda = true_lambda)

# Step 2: Define prior on λ ~ Gamma(α, β)
a_prior <- 2
b_prior <- 1

# Step 3: Posterior update
a_post <- a_prior + sum(data)
b_post <- b_prior + n

# Step 4: Simulate from prior and posterior
lambda_prior_samples <- rgamma(10000, shape = a_prior, rate = b_prior)
lambda_post_samples <- rgamma(10000, shape = a_post, rate = b_post)

# Step 5: Posterior predictive simulation
y_pred <- rpois(10000, lambda = lambda_post_samples)

# Step 6: MAP estimator
map_estimate <- if (a_post > 1) (a_post - 1) / b_post else NA

# Step 7: 95% Credible Interval and HDI
cred_int <- quantile(lambda_post_samples, c(0.025, 0.975))
hdi_int <- hdi(lambda_post_samples, credMass = 0.95)

# Print results
cat("MAP Estimate of λ:", round(map_estimate, 4), "\n")
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
  lambda_prior_samples,
  breaks = 50,
  freq = FALSE,
  col = "orchid1",
  main = paste0("Prior: Gamma(", a_prior, ", ", b_prior, ")"),
  xlab = expression(lambda)
)

text(
  x = 7,
  y = 0.29,
  labels = expression(
    atop(
      alpha[post] == alpha[prior] + sum(y),
      beta[post] == beta[prior] + n
    )
  ),
  adj = 0,
  cex = 0.9
)

# 2. Posterior distribution with MAP, CI, and HDI
hist(
  lambda_post_samples,
  breaks = 50,
  freq = FALSE,
  col = "lightblue",
  main = paste0("Posterior: Gamma(", a_post, ", ", b_post, ")"),
  xlab = expression(lambda)
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

# 3. Posterior predictive vs true Poisson
max_y <- max(y_pred, qpois(0.999, lambda = true_lambda))
breaks_vals <- seq(-0.5, max_y + 0.5, by = 1)

hist(
  y_pred,
  breaks = breaks_vals,
  freq = FALSE,
  col = "gold",
  border = "white",
  main = "Posterior Predictive vs True Poisson",
  xlab = "y",
  xlim = c(0, max_y)
)

# Overlay the true Poisson PMF
y_vals <- 0:max_y
true_probs <- dpois(y_vals, lambda = true_lambda)
points(y_vals, true_probs, type = "h", col = "blue", lwd = 2)
points(y_vals, true_probs, col = "blue", pch = 16)

legend(
  "topright",
  inset = c(-0.3, 0),
  xpd = TRUE,
  legend = c(
    "Posterior Predictive",
    bquote("Poisson(" ~ lambda == .(true_lambda) ~ ")")
  ),
  fill = c("gold", NA),
  border = c("white", NA),
  col = c("gold", "blue"),
  pch = c(NA, 16),
  lty = c(NA, 1),
  lwd = c(NA, 2),
  bty = "n"
)

# 4. Overlay prior and posterior for comparison
plot(
  density(lambda_prior_samples),
  col = "orchid4",
  lwd = 2,
  main = paste0(
    "Prior vs Posterior\nGamma(",
    a_prior,
    ",",
    b_prior,
    ") vs Gamma(",
    a_post,
    ",",
    b_post,
    ")"
  ),
  xlab = expression(lambda),
  ylim = c(0, max(density(lambda_post_samples)$y))
)

lines(density(lambda_post_samples), col = "blue", lwd = 2)
legend(
  "topright",
  legend = c("Prior", "Posterior"),
  col = c("orchid4", "blue"),
  lwd = 2,
  bty = "n"
)
