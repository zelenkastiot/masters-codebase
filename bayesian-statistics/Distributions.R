################################################### Different Beta (parmaters)
# Beta Distributions
x <- seq(0, 1, length.out = 100)

plot(
  x,
  dbeta(x, 2, 5),
  type = "n",
  ylim = c(0, 3),
  ylab = "Density",
  xlab = "x",
  main = "Beta Distributions"
)

lines(x, dbeta(x, 0.5, 0.5), col = "red", lwd = 2)
lines(x, dbeta(x, 5, 1), col = "green", lwd = 2)
lines(x, dbeta(x, 1, 3), col = "blue", lwd = 2)
lines(x, dbeta(x, 2, 2), col = "purple", lwd = 2)
lines(x, dbeta(x, 2, 5), col = "gold", lwd = 2)

legend(
  "topright",
  legend = c("α=β=0.5", "α=5, β=1", "α=1, β=3", "α=2, β=2", "α=2, β=5"),
  col = c("red", "green", "blue", "purple", "gold"),
  lwd = 2
)

################################################### Histogram
set.seed(123)

alpha <- 2
beta <- 5

samples <- rbeta(10000000, shape1 = alpha, shape2 = beta) # random sample

hist(
  samples,
  breaks = 100,
  probability = TRUE,
  col = "skyblue",
  border = "white",
  main = "Histogram of Beta(2, 5) sample",
  xlab = "x",
  xlim = c(0, 1)
)

curve(dbeta(x, alpha, beta), add = TRUE, col = "darkblue", lwd = 2) # plot pdf

legend(
  "topright",
  legend = c("Histogram", "PDF"),
  fill = c("skyblue", NA),
  border = c("white", NA),
  lty = c(NA, 1),
  lwd = c(NA, 2),
  col = c("skyblue", "darkblue")
)

################################################### Free throws example
curve(dbeta(x, 2, 3))
curve(dbeta(x, 20, 30))
curve(dbeta(x, 20, 33))

qbeta(c(0.05, .95), 30, 23)

source("Desktop/masters-codebase/bayesian-statistics/BetaParmsFromQuantiles.R")
beta.parms.from.quantiles(c(.6, .9), plot = T)
