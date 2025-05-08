curve(
  1 + 0 * x,
  from = 0,
  to = 1,
  col = "black",
  lty = 2,
  ylim = c(0, 2),
  ylab = "Density",
  xlab = "θ",
  main = "Prior Densities for θ"
)
curve(x + 0.5, from = 0, to = 1, col = "blue", add = TRUE)
curve(2 * x, from = 0, to = 1, col = "red", add = TRUE)
legend(
  "topleft",
  legend = c("Uniform (1)", "θ + 0.5", "2θ"),
  col = c("black", "blue", "red"),
  lty = c(2, 1, 1)
)
