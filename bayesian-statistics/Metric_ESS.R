# -----------------------------------------------
# Effective Sample Size (ESS)
#
# ESS quantifies how many independent samples our
# (possibly autocorrelated) sample set is equivalent to.
#
# In MCMC sampling, ESS is often much smaller than
# the raw number of samples due to autocorrelation.
#
# Formula (conceptual):
#   ESS = N / (1 + 2 * sum of autocorrelations)
#
# Interpretation:
#   - High ESS ≈ good sampling, low autocorrelation
#   - Low ESS ≈ poor sampling, high autocorrelation
#
# If samples are i.i.d. (e.g., from rbeta), ESS ≈ N.
# For MCMC, we use tools like coda::effectiveSize().
# -----------------------------------------------
library(coda)

n <- 10000
samples <- rbeta(n, 3, 11) # or your MCMC samples

ess <- effectiveSize(samples)
print(ess)
