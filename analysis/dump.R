View(rhemcon)
sim_plot <- ggplot2::ggplot(data = sim_coef_df_filtered) +
  ggplot2::geom_point(ggplot2::aes(x = cat, y = lambda_03 ))

aggregate_coef <- sim_coef_df %>% 
  dplyr::select(`f1=~y1`,`f2=~y6`, `f1=~y5`, `f2=~y10`, N, sym, cat, dist) %>% rowwise() %>% 
  dplyr::mutate(lambda_03 = mean(c(`f1=~y1`, `f2=~y6`), na.rm = TRUE)) %>%   
  dplyr::group_by(N, cat, sym, dist) %>% mutate(lambda_03_bar = mean(lambda_03, na.rm = TRUE)) %>% 
  mutate(bias_03 = lambda_03_bar - 0.3)

  
sim_reps_agg <- c(sim_reps, sim_reps2) %>% bind_rows %>% 
  dplyr::group_by(N, cat, sym, dist) %>% 
  summarize(avg_bias = mean(bias_03, na.rm = TRUE),
            avg_parameter = mean(lambda_03_bar, na.rm = TRUE))

  glimpse(aggregate_coef)
  
  
  sim_plot <- ggplot2::ggplot(data = aggregate_coef) +
    ggplot2::geom_point(ggplot2::aes(x = cat, y = bias_03 ))
  
  
  model_table <- lavaanify(model = model_sim)
  sim_model1
  
  model_table_alt <- lavParTable(model = model_sim)
  
  
  plotDist(bindDist(skewness = 2, kurtosis = 7))
  1:6 %>% map(~hist(gen_data[,.x]))
  

# More old stuff ----------------------------------------------------------

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

# Dump from run_sim -------------------------------------------------------

  # Estimate model on simulated data
  
  # sim_raw_res <- simsem::sim(model = sim_model, rawData = sim_raw_2$cat_data)
  
  
  # # Extract model coefficients from estimated models
  # sim_coef <- coef(sim_raw_res)
  # 
  # 
  # # Add id variable based on row names --------------------------------------
  # 
  # sim_coef_id <- sim_coef %>% 
  #   tibble::rownames_to_column(var = "id") %>% 
  #   mutate(id = as.numeric(id))
  # 
  # # Join scenarios and estimated coefficients data frame --------------------
  # 
  # sim_coef_df <- dplyr::left_join(sim_scenarios, sim_coef_id)
  # 
  # vars_01 <- c("f1=~y1", "f2=~y6")
  # vars_02 <- c("f1=~y5", "f2=~y10")
  # 
  # aggregate_coef <- sim_coef_df %>% 
  #   dplyr::select(`f1=~y1`,`f2=~y6`, `f1=~y5`, `f2=~y10`, N, sym, cat, dist) %>% 
  #   rowwise() %>% 
  #   dplyr::mutate(lambda_03 = mean(c(`f1=~y1`, `f2=~y6`), na.rm = TRUE)) %>%   
  #   dplyr::group_by(N, cat, sym, dist) %>% 
  #   mutate(lambda_03_bar = mean(lambda_03, na.rm = TRUE)) %>% 
  #   mutate(bias_03 = lambda_03_bar - 0.3) %>% 
  #   ungroup()
  # 
  # aggregate_coef
  
  # sim_raw_res  
  

# Dunp from inspecting outcome from cfa_functions -------------------------

  
  
  glimpse(sim_raw_fit)
  
  head(sim_raw_fit)
  sim_raw_fit2
  
  sim_raw_fit %>% summarise(across(b1:s1, mean))
  
  
  sim_raw_fit22 <- sim_raw_fit2 %>% map_df(~{.x[1:11]})
  sim_raw_fit22 %>% summarise(across(b1:s1, mean))
  
  
  sim_raw_fit2[[22]]
  
  sim_raw_fit22 <- do.call(rbind,sim_raw_fit2)
  
  sim_raw_fit2 %>% map_dbl(length)
  
  sim_raw_fit2$fit %>% head
  
  sim_raw_fit$test %>% head
  
  fit$pvalue.scaled
  
  
  $scaled.shifted$pvalue # for type 1 error
  
  sim_raw_fit$parameter_est[[9]]
  
  sim_raw_fit2$options[[1]]
  
  sum(sim_raw_fit$converged)
  
  sim_reps4[[1]] %>% glimpse
  
  paste0("b",1:20)
  
  object.size(sim_raw_fit) %>% print(humanReadable = TRUE)  
  
  
  sim_fit_ML <- do.call(bind_rows,sim_raw_fit)
  sim_fit_ML <- tibble(estimator = "ML", sim_fit_ML)
  
  sim_fit_ULS <- do.call(bind_rows,sim_raw_fit2)
  sim_fit_ULS <- tibble(estimator = "ULS", sim_fit_ULS)
  
  sim_fit_all <- bind_rows(sim_fit_ML, sim_fit_ULS)
  

# Check whether sample covariance matrix is not positive-definite ---------

cov_test <- cov(sim_reps[[11]]$cat_data[[18]])  
  
eigen(cov_test, symmetric = TRUE)

  glimpse(sim_reps)
  
  
MASS::mvrnorm()

cov_test2 <- class(try(inspectSampleCov(model = model1, data = sim_reps[[1]]$cat_data[[18]]), 
                       silent = T)) == "try-error"


foo <- 1+1

eigen(cov_test2$cov, symmetric = TRUE)

str(cov_test2)

class(cov_test2)

rm(cov_test2)

bar <- "model1"

get(bar)

var_test <- sim_reps[[1]]$cat_data[[18]] %>% 
  summarize_all(.funs = var) %>% 
  t %>% min

pos_def_test <- class(try(inspectSampleCov(model = get("model1"), 
                                      data = sim_reps[[1]]$cat_data[[18]]), 
                     silent = T)) == "try-error"

var_test==0 & pos_def_test

sim_reps %>% map(~{print(paste0(nrow(.x$sim_data), .x$run_id))})


# Figuring out how to handle errors ---------------------------------------

test_tbl <- tibble(a = c(1:3),
                   b = c(TRUE, TRUE, FALSE),
                   c = list(c(5,4,3), c(3,2,1), c(1,2)))

test_tbl2 <- NULL

bind_rows(test_tbl, NULL)

test_list <- list(test_tbl, test_tbl, test_tbl2, test_tbl)

test_result <- test_list %>% map_df(~{.x})



# testing -----------------------------------------------------------------

cfa_ULS(run_id = sim_reps50[[1]]$run_id, 
            id = sim_reps50[[1]]$sim_data$id[1], 
            models = sim_reps50[[1]]$sim_data$models[1], 
            cat_data = sim_reps50[[1]]$sim_data$cat_data[[1]])

fitULS(sim_reps50[[1]]) 


# Filter out errounous runs -----------------------------------------------

scaled.shifted

sim_fit_all_joined$test %>% map_lgl(~is.null(.x$scaled.shifted))

sim_fit_all_joined$test[[18]]

sim_fit_all_joined[18,]

test_lavaan_res <- lavaan::cfa(model = get(sim_reps50[[2]]$sim_data$models[1]),
            data = sim_reps50[[2]]$sim_data$cat_data[[1]],
            std.lv = TRUE, 
            orthogonal = FALSE,
            estimator = "MLMV")


lavInspect(object = test_lavaan_res, 
           what = "std")

lavInspect(object = test_lavaan_res, 
           what = "se")

extract_par_est(test_lavaan_res)

extract_par_est <- function(modfit){
  load <- lavInspect(object = modfit, 
                     what = "std")$lambda %>% 
    as_tibble %>% 
    rownames_to_column(var = "label") %>% 
    rowwise() %>% 
    transmute(label = paste0("b", label),
              est = sum(f1, f2))
  
  s1 <- tibble(label = "s1",
               est = lavInspect(object = modfit, 
                                what = "std")$psi[2,1])
  
  par_est <- bind_rows(load, s1)
  
  se_load <- lavInspect(object = modfit, 
                        what = "se")$lambda %>% 
    as_tibble %>% 
    rownames_to_column(var = "label") %>% 
    rowwise() %>% 
    transmute(label = paste0("b", label),
              se = sum(f1, f2))
  
  se_s1 <- tibble(label = "s1",
                  se = lavInspect(object = modfit, 
                                  what = "se")$psi[2,1])
  
  
  se <- bind_rows(se_load, se_s1)
  
  left_join(par_est, se)
}







parameter_est <- lavInspect(object = test_lavaan_res, 
                            what = "list") %>% 
  dplyr::select(label, est, se) %>% 
  dplyr::filter(label %in% c(paste0("b", 1:20), "s1"))


lavInspect(object = test_lavaan_res, 
           what = "list") %>% 
  dplyr::select(label, est, se) %>% 
  dplyr::filter(label %in% c(paste0("b", 1:20), "s1"))

lavInspect(object = test_lavaan_res, 
           what = "se")

label    est    se
1     b1  0.051 0.060
2     b2  0.252 0.053
3     b3  0.274 0.052
4     b4  0.269 0.051
5     b5  0.329 0.050
6     b6 -0.017 0.063
7     b7  0.211 0.062
8     b8  0.248 0.059
9     b9  0.311 0.062
10   b10  0.262 0.063
11    s1  0.199 0.153

$lambda
f1     f2
y1  0.102  0.000
y2  0.513  0.000
y3  0.550  0.000
y4  0.538  0.000
y5  0.658  0.000
y6  0.000 -0.035
y7  0.000  0.424
y8  0.000  0.499
y9  0.000  0.626
y10 0.000  0.525

$lambda
f1    f2
y1  0.060 0.000
y2  0.053 0.000
y3  0.052 0.000
y4  0.051 0.000
y5  0.050 0.000
y6  0.000 0.063
y7  0.000 0.062
y8  0.000 0.059
y9  0.000 0.062
y10 0.000 0.063



test_lavaan_resULS <- lavaan::cfa(model = get(sim_reps50[[2]]$sim_data$models[1]),
                               data = sim_reps50[[2]]$sim_data$cat_data[[1]],
                               std.lv = TRUE, 
                               orthogonal = FALSE,
                               estimator = "ULSMV",
                               ordered = TRUE)

est <- lavInspect(object = test_lavaan_resULS, 
           what = "std")$lambda %>% as_tibble %>% 
  rownames_to_column(var = "label") %>% rowwise() %>% 
  mutate(est = sum(f1, f2)) %>% 
  mutate(label = paste0("b", label))

tibble(label = s1,
       est = lavInspect(object = test_lavaan_resULS, 
           what = "std")$psi[2,1])




lavInspect(object = test_lavaan_resULS, 
           what = "list") %>% 
  dplyr::select(label, est, se) %>% 
  dplyr::filter(label %in% c(paste0("b", 1:20), "s1"))

lavInspect(object = test_lavaan_resULS, 
           what = "se")

$lambda
f1     f2
y1  0.154  0.000
y2  0.606  0.000
y3  0.662  0.000
y4  0.655  0.000
y5  0.836  0.000
y6  0.000 -0.066
y7  0.000  0.463
y8  0.000  0.629
y9  0.000  0.771
y10 0.000  0.688


$psi
f1    f2   
f1 1.000      
f2 0.207 1.000

label    est    se
1     b1  0.154 0.153
2     b2  0.606 0.126
3     b3  0.662 0.121
4     b4  0.655 0.112
5     b5  0.836 0.106
6     b6 -0.066 0.165
7     b7  0.463 0.152
8     b8  0.629 0.142
9     b9  0.771 0.150
10   b10  0.688 0.160
11    s1  0.207 0.159

$lambda
f1    f2
y1  0.153 0.000
y2  0.126 0.000
y3  0.121 0.000
y4  0.112 0.000
y5  0.106 0.000
y6  0.000 0.165
y7  0.000 0.152
y8  0.000 0.142
y9  0.000 0.150
y10 0.000 0.160

$psi
f1    f2   
f1 0.000      
f2 0.159 0.000