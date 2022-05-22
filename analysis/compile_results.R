
# Read in results ---------------------------------------------------------

library(tidyverse)

sim_scenarios_id <- readRDS("sim_scenarios_id.rds")

simfitULS <- bind_rows(simfitULS1, sim_fitULS2)


c(2:4, 6:11) %>% walk(~{load(paste0("./results/simfitULS", .x, ".rds"),
                     envir = .GlobalEnv)})

12:20 %>% walk(~{readRDS(paste0("./results/simfitULS", .x, ".rds"))})

simfitULS5<- readRDS("./results/simfitULS5.rds")
simfitULS1<- readRDS("./results/simfitULS1.rds")
simfitULS12<- readRDS("./results/simfitULS12.rds")
simfitULS13<- readRDS("./results/simfitULS13.rds")
simfitULS14<- readRDS("./results/simfitULS14.rds")
simfitULS15<- readRDS("./results/simfitULS15.rds")
simfitULS16<- readRDS("./results/simfitULS16.rds")
simfitULS17<- readRDS("./results/simfitULS17.rds")
simfitULS18<- readRDS("./results/simfitULS18.rds")
simfitULS19<- readRDS("./results/simfitULS19.rds")
simfitULS20<- readRDS("./results/simfitULS20.rds")


data_list <- list(simfitULS1, simfitULS2, simfitULS3, simfitULS4, 
               simfitULS5, simfitULS6, simfitULS7, simfitULS8, 
               simfitULS9, simfitULS10, simfitULS11, simfitULS12, 
               simfitULS13, simfitULS14, simfitULS15, simfitULS16, 
               simfitULS17, simfitULS18, simfitULS19, simfitULS20)



simfitULS <- do.call(bind_rows, data_list)

rm(list = paste0("simfitULS", 1:20))
rm(data_list)

simfitULS <- tibble(estimator = "ULS", simfitULS)

simfitULS <- simfitULS %>% 
  left_join(sim_scenarios_id, 
            by = c("scenario_id" = "id"))

saveRDS(simfitULS, "./results/simfitULS")

simfitML <- readRDS("./simfitML.rds")

simfitML <- simfitML %>% 
  left_join(sim_scenarios_id, 
            by = c("scenario_id" = "id"))

simfitML <- tibble(estimator = "ML", simfitML)

saveRDS(simfitML, "./results/simfitML")

simfitALL <- bind_rows(simfitML, simfitULS)
saveRDS(simfitALL, "./results/simfitALL.rds")