# -----------------------------------------------
# Highest Density Interval (HDI)
#
# HDI is a Bayesian interval estimate that contains
# the most credible values of a parameter.
#
# Properties:
#   - Always contains the mode (MAP)
#   - All values inside the HDI have higher probability
#     density than any value outside
#   - Especially useful for skewed or multimodal posteriors
#
# Use the HDInterval package to compute:
#   hdi(qbeta, shape1, shape2, credMass = 0.95)
# -----------------------------------------------
library(HDInterval)
# Posterior parameters
alpha_post <- 2
beta_post <- 12

# eg. 95% HDI
hdi_vals <- hdi(qbeta, shape1 = alpha_post, shape2 = beta_post, credMass = 0.95)

# -----------------------------------------------
# -----------------------------------------------
# Credible Interval (for posterior central mass)
#
# A Bayesian analogue to the confidence interval.
#
# A 95% credible interval means:
#   "There's a 95% probability that the parameter lies
#    within this interval, given the data."
#
# Typically based on quantiles:
#   qbeta(c(0.025, 0.975), alpha, beta)
#
# Unlike HDI, this interval may not include the mode
# in skewed distributions.
# -----------------------------------------------

# eg. 95% Credible Interval (quantiles)
ci_vals <- qbeta(c(0.025, 0.975), alpha_post, beta_post)

# -----------------------------------------------
# -----------------------------------------------

# Grid for plotting
x_vals <- seq(0, 1, length.out = 1000)
y_vals <- dbeta(x_vals, alpha_post, beta_post)

# Plot posterior density
plot(
  x_vals,
  y_vals,
  type = "l",
  lwd = 2,
  xlab = expression(theta),
  ylab = "Density",
  main = "Posterior Beta(2, 8) with HDI and Credible Interval"
)

# Shade HDI (red)
polygon(
  c(
    hdi_vals[1],
    x_vals[x_vals >= hdi_vals[1] & x_vals <= hdi_vals[2]],
    hdi_vals[2]
  ),
  c(0, y_vals[x_vals >= hdi_vals[1] & x_vals <= hdi_vals[2]], 0),
  col = rgb(1, 0, 0, 0.3),
  border = NA
)

# Shade Credible Interval (blue)
polygon(
  c(
    ci_vals[1],
    x_vals[x_vals >= ci_vals[1] & x_vals <= ci_vals[2]],
    ci_vals[2]
  ),
  c(0, y_vals[x_vals >= ci_vals[1] & x_vals <= ci_vals[2]], 0),
  col = rgb(0, 0, 1, 0.2),
  border = NA
)

# Add vertical lines
abline(v = hdi_vals, col = "red", lty = 3, lwd = 2)
abline(v = ci_vals, col = "blue", lty = 2, lwd = 2)

# Add legend
legend(
  "topright",
  legend = c("95% HDI", "95% Credible Interval"),
  fill = c(rgb(1, 0, 0, 0.3), rgb(0, 0, 1, 0.2)),
  border = NA,
  lty = c(3, 2),
  col = c("red", "blue"),
  lwd = 2,
  bty = "n"
)
