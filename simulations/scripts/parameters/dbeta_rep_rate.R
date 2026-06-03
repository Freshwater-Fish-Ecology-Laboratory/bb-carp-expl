mean_l <- 0.70
var_l <- 0.019

# These approaches yield equivalent estimates of alpha and beta
# (1)
alpha <- ((1 - mean_l)/ var_l - 1 / mean_l) * mean_l^2
beta <- alpha* (1 / mean_l - 1)
# (2)
alpha <- (mean_l^2-mean_l^3-mean_l*var_l)/ var_l
beta <- (mean_l-2*mean_l^2 + mean_l^3 - var_l + mean_l*var_l) / var_l

###
alpha
beta

n_samples <- 1000
samples <- rbeta(n_samples, alpha, beta)

hist(samples, freq = FALSE, main = "Beta Distribution", xlab = "Reporting rate value", ylab = "Density")

curve(dbeta(x, alpha, beta), add = TRUE, col = "navy", lwd = 2)


# why does the scaling of alpha and beta change the shape of the curve??
# need to specify the sample size (what sample - tagged fish? tag reports?) and multiply alpha and beta
# by this number?
