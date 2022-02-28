# Model definition

model_sim <- "f1 =~ 0.3 * y1 + 0.4 * y2 + 0.5 * y3 + 0.6 * y4 + 0.7* y5
            f2 =~ 0.3 *y6 + 0.4 * y7 + 0.5 * y8 + 0.6 * y9 + 0.7 * y10
            f1 ~~ 0.3 * f2
            f1 ~~ 1 * f1
            f2 ~~ 1 * f2"

names(sim_raw)

model1 <- "f1 =~ b1 * y1 + b2 * y2 + b3 * y3 + b4 * y4 + b5 *y5
            f2 =~ b6 * y6 + b7 * y7 + b8 * y8 + b9 * y9 + b10 * y10
            f1 ~~ s1 * f2
            f1 ~~ 1 * f1
            f2 ~~ 1 * f2"

# Define model 1 ----------------------------------------------------------
loadings <- matrix(0, 10, 2)
loadings[1:5, 1] <- NA
loadings[6:10, 2] <- NA
pop <- matrix(0, 10, 2)
pop[1:5, 1] <- c(.3, .4, .5, .6, .7)
pop[6:10, 2] <- c(.3, .4, .5, .6, .7)

sim_model1 <- getModel(N = 100, 
                       loadings = loadings, 
                       population = pop, 
                       latent.cor = 0.3)

# Define model 2 ----------------------------------------------------------
loadings <- matrix(0, 10, 2)
loadings[1:5, 1] <- NA
loadings[6:10, 2] <- NA
pop <- matrix(0, 10, 2)
pop[1:5, 1] <- c(.3, .4, .5, .6, .7)
pop[6:10, 2] <- c(.3, .4, .5, .6, .7)

sim_model1 <- getModel(N = 100, 
                       loadings = loadings, 
                       population = pop, 
                       latent.cor = 0.3)