xbars = c()
observations = c()

true_mean = 2
true_sd = 1
no_samples = 10000

for (n in 1:no_samples){
  newdata = rnorm(1, mean = true_mean, sd = true_sd ) 
  # rcauchy(n) # this dist has /inf mean, will never converge
  observations = c(observations, newdata)
  xbars = c(xbars, mean(observations))
}

plot(xbars, 
     type = "l", 
     col = "blue", 
     xlab = "Number of Observations", 
     ylab = "Running Mean", 
     main = paste("Convergence of Sample Mean to True Mean=", true_mean, " with", no_samples, "samples"))

abline(h = 2, col = "red", lty = 2)  # true mean line
