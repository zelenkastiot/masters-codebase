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
curve(dbeta(x, 2, 3), col = "blue", add = TRUE)
curve(dbeta(x, 20, 30), col = "red", add = TRUE)
curve(dbeta(x, 20, 33), col = "green", add = TRUE)

qbeta(c(0.05, .95), 30, 23)

source("Desktop/masters-codebase/bayesian-statistics/BetaParmsFromQuantiles.R")
beta.parms.from.quantiles(c(.6, .9), plot = T)

################################################### Binomial
## Example 1: fixed theta

Y1 <- rbinom(1000, 10, 0.1)
Y2 <- rbinom(1000, 10, 0.2)
Y3 <- rbinom(1000, 10, 0.3)

par(mfrow = c(1, 3))

hist(
  Y1,
  xlab = "Y1 Bin ( 10 , 0.1 )",
  col = "gold",
  main = expression(theta == 0.1)
)
hist(
  Y2,
  xlab = "Y2 Bin ( 10 , 0.2 )",
  col = "forestgreen",
  main = expression(theta == 0.2)
)
hist(
  Y3,
  xlab = "Y3 Bin ( 10 , 0.3 )",
  col = "red",
  main = expression(theta == 0.3)
)
par(oma = c(0, 0, 2, 0))

title(
  "Number of sunny days ( samples of 1000), for different theta",
  outer = TRUE
)

## Example 2: theta from PDF
par(mfrow = c(1, 1))
theta <- rbeta(1000, 3, 11)

Y = c()
for (i in theta) {
  r <- rbinom(100, 10, i)
  Y = c(Y, r)
}

hist(Y, main = "", freq = F)

hist(theta, freq = T)
curve(dbeta(x, 3, 11), col = "red", add = TRUE)

library(extraDistr)
points(
  dbbinom(x, size = 10, alpha = 3, beta = 11),
  add = TRUE,
  col = "darkblue",
  lwd = 2
)
