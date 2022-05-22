# Power estimation

power_scenarios <- sim_scenarios_id %>% 
  dplyr::filter(models == "model1",
                dist == "normal",
                sym == "sym") 

# Generate data -----------------------------------------------------------
set.seed(8361)
sim_reps <- readRDS("simreps.rds")


runPowerML <- function(datalist){datalist %>% 
    map_df(~{.x$sim_data %>% 
        dplyr::filter(models == "model1",
                      dist == "normal",
                      sym == "sym") %>% 
        dplyr::select(id, cat_data) %>%  
        pmap(possPowerML)})
}

PowerML <-  function(id, cat_data){
  fit <- lavaan::cfa(model = get("model3"),
                     data = cat_data,
                     std.lv = TRUE, 
                     orthogonal = FALSE,
                     estimator = "MLMV")
  pvalue <- fitmeasures(fit)["pvalue.scaled"]
  
  tibble(id = id,
         est = "ML",
         sig = pvalue < 0.05)}

possPowerML <- possibly(.f = PowerML,
                        otherwise = NULL)


runPowerULS <- function(datalist){datalist %>% 
    map_df(~{.x$sim_data %>% 
        dplyr::filter(models == "model1",
                      dist == "normal",
                      sym == "sym") %>% 
        dplyr::select(id, cat_data) %>%  
        pmap(possPowerULS)})
}

PowerULS <- function(id, cat_data){
  fit <- lavaan::cfa(model = get("model3"),
                     data = cat_data,
                     std.lv = TRUE, 
                     orthogonal = FALSE,
                     estimator = "ULSMV",
                     ordered = TRUE)
  pvalue <- fitmeasures(fit)["pvalue.scaled"]
  
  tibble(id = id,
         est = "cat-LS",
         sig = pvalue < 0.05)}

possPowerULS <- possibly(.f = PowerULS,
                        otherwise = NULL)




sim_powerML <- runPowerML(sim_reps)

saveRDS(sim_powerML, "sim_powerML.rds")

sim_powerULS <- runPowerULS(sim_reps)
saveRDS(sim_powerULS, "sim_powerULS.rds")

powerML <- sim_powerML %>% 
  group_by(id) %>% 
  summarize(power = mean(sig))

powerULS <- sim_powerULS %>% 
  group_by(id) %>% 
  summarize(power = mean(sig))



power_dfML <- powerML %>% 
  left_join(sim_scenarios_id, by = "id")

power_dfML <- tibble(est = "ML",
                     power_dfML)

power_dfULS <- powerULS %>% 
  left_join(sim_scenarios_id, by = "id")


power_dfULS <- tibble(est = "ULS",
                     power_dfULS)


powerdf <- bind_rows(power_dfML, power_dfULS)
# Table -------------------------------------------------------------------

library(kableExtra)

powerdf_wide <- powerdf %>% 
  dplyr::select(est, power, N, cat) %>% 
  arrange(cat) %>% 
  pivot_wider(names_from = c(est, cat),
              values_from = c(power))

kbl(powerdf_wide %>% round(digits = 3),
    col.names = c("N", rep(c("ML", "ULS"), 6))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 1, 
                     "2 categories" = 2,
                     "3 categories" = 2,
                     "4 categories" = 2,
                     "5 categories" = 2,
                     "6 categories" = 2,
                     "7 categories" = 2)) %>% 
  footnote(general = "Type I error was assessed by fitting a one-factor model to two-factor simulated data. ML = robust continuous maximum likelihood estimation; ULS = robust categorical least squares estimation.")# %>% landscape()