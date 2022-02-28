gen_cat_data <- function(models, N, cat, sym, dist, ...){
  set.seed(seed)
  gen_data <- simsem::generate(model = models, 
                               n = N,
                               indDist = switch(dist,
                                                "normal" = NULL,
                                                "non-normal" = simsem::bindDist(skewness = 2, kurtosis = 7))
  )
  cat_data <- catData(gen_data, 
                      k = cat, 
                      sym = sym, 
                      standardize = FALSE)
  cat_data
}

# list of all arguments used to run lavaan

sim_raw_fit <- sim_raw %>% map_df(~{fit <- lavaan::cfa(model = model1,
                                           data = .x,
                                           std.lv = TRUE, 
                                           orthogonal = FALSE)
                                coef(fit)})

sim_coef <- tibble(sim_scenarios_id, sim_raw_fit)

sim_coef %>% filter(id == "1")

lav_mod <- list(model = model1,
     data = ,
     std.lv = TRUE, 
     orthogonal = FALSE,
     estimator = "ML")

# or estimator = "ULS" (unweighted least squares")


my_data <- sim(nRep = 2,
               n = 100,
               model = sim_model1,
               generate = list(model = model_sim,
                               model.type = "cfa",
                               std.lv = TRUE,
                               sample.nobs = 100L,
                               skewness = 2,
                               kurtosis = 7,
                               seed = 42,
                               return.fit = TRUE))
summaryPopulation(my_data)


fit3 <- sim_raw %>% map(~analyze(model = sim_model1, 
                                data = .x, 
                                package = "lavaan", 
                                estimator = "MLMV", 
                                se ="robust.sem",
                                test = "Satorra.Bentler",
                                model.type = "cfa"))
                       
fit2 <- sim_raw %>% map(~analyze(model = sim_model1, 
                                 data = .x, 
                                 package = "lavaan", 
                                 estimator = "ULSMV",
                                 ordered = TRUE))
fit_coef2 <- fit2 %>% map(coef)

fit_coef3 <- fit3 %>% map(~summary(object = .x,
                                estimates = TRUE,
                               ci = TRUE,
                               standardized = TRUE))

#           mimic = "Mplus")
head(fit_coef2)

lavInspect(object = fit2[[2]], 
           what = "converged")

lavInspect(object = fit2[[2]], 
           what = "post.check")

lavInspect(object = fit2[[2]], 
           what = "options")
