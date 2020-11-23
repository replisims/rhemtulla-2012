# Recreate the Rhemtulla conditions

# Distributions
normal <- bindDist("norm", list(mean = 0, sd = 1))
nonnormal <- bindDist(skewness = 2, kurtosis = 7)

# Model A - 10 indicators
loading <- matrix(0, 10, 2)
loading[1:5, 1] <- NA
loading[6:10, 2] <- NA

pop <- matrix(0, 10, 2)
pop[1:5, 1] <- c(.3, .4, .5, .6, .7)
pop[6:10, 2] <- c(.3, .4, .5, .6, .7)

modA <- getModel(loadings = loading, 
                 population = pop,
                 latent.cor = 0.3)

# Model B - 20 indicators
loading <- matrix(0, 20, 2)
loading[1:10, 1] <- NA
loading[11:20, 2] <- NA

pop <- matrix(0, 20, 2)
pop[1:10, 1] <- rep(c(.3, .4, .5, .6, .7), 2)
pop[11:20, 2] <- rep(c(.3, .4, .5, .6, .7), 2)

modB <- getModel(loadings = loading, 
                 population = pop,
                 latent.cor = 0.3)

rhemcon <- expand_grid(N = c(100, 150, 350, 600), 
                       cat = 2:7, 
                       sym = c("sym", "moderate", "moderate-alt", 
                                      "extreme", "extreme-alt"), 
                       models = c(modA, modB),
                       dist = c(normal, nonnormal))

save(rhemcon, file = "data/rhemcon.rda", compress = "xz")
