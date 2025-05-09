# Load required libraries
library(HDInterval)
library(extraDistr)

# Step 1: Simulate observed data from Exponential(λ)
set.seed(42)
true_lambda <- 2 # true rate
n <- 100
data <- rexp(n, rate = true_lambda)

# Step 2: Define prior on λ ~ Gamma(α, β)
a_prior <- 2
b_prior <- 1

# Step 3: Posterior update
a_post <- a_prior + n
b_post <- b_prior + sum(data)

# Step 4: Simulate from prior and posterior
lambda_prior_samples <- rgamma(10000, shape = a_prior, rate = b_prior)
lambda_post_samples <- rgamma(10000, shape = a_post, rate = b_post)

# Step 5: Posterior predictive simulation
y_pred <- rexp(10000, rate = lambda_post_samples)

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
  col = "lightgray",
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
  main = paste0("Posterior: Gamma(", a_post, ", ", round(b_post, 1), ")"),
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

# 3. Posterior predictive vs true Exponential
hist(
  y_pred,
  breaks = 50,
  freq = FALSE,
  col = "orange",
  border = "white",
  main = "Posterior Predictive vs True Exponential",
  xlab = "y",
  xlim = c(0, quantile(y_pred, 0.99))
)

# Overlay true Exponential PDF
x_vals <- seq(0, quantile(y_pred, 0.99), length.out = 300)
true_density <- dexp(x_vals, rate = true_lambda)
lines(x_vals, true_density, col = "blue", lwd = 2)
legend(
  "topright",
  inset = c(-0.3, 0),
  xpd = TRUE,
  legend = c(
    "Posterior Predictive",
    bquote("Exp(" ~ lambda == .(true_lambda) ~ ")")
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
  density(lambda_prior_samples),
  col = "gray40",
  lwd = 2,
  main = paste0(
    "Prior vs Posterior\nGamma(",
    a_prior,
    ",",
    b_prior,
    ") vs Gamma(",
    a_post,
    ",",
    round(b_post, 1),
    ")"
  ),
  xlab = expression(lambda),
  ylim = c(0, max(density(lambda_post_samples)$y))
)
lines(density(lambda_post_samples), col = "blue", lwd = 2)
legend(
  "topright",
  legend = c("Prior", "Posterior"),
  col = c("gray40", "blue"),
  lwd = 2,
  bty = "n"
)
