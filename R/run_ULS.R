fitULS <- function(datalist){datalist %>% 
    map_df(~{.x$sim_data %>% 
        dplyr::select(id, models, cat_data) %>%  
        pmap_dfr(.f = function(id, models, cat_data){rhemtulla2012:::posscfa_ULS(run_id = .x$run_id, 
                                                                                 id = id, 
                                                                                 models = models, 
                                                                                 cat_data = cat_data)})})
}


set.seed(6102)
simfitULS12 <- fitULS(simrep12)

saveRDS(object = simfitULS12,
        file = "simfitULS12.rds")

set.seed(6102)
simfitULS13 <- fitULS(simrep13)

saveRDS(object = simfitULS13,
        file = "simfitULS13.rds")

set.seed(6102)
simfitULS14 <- fitULS(simrep14)

saveRDS(object = simfitULS14,
        file = "simfitULS14.rds")

set.seed(6102)
simfitULS15 <- fitULS(simrep15)

saveRDS(object = simfitULS15,
        file = "simfitULS15.rds")

set.seed(6102)
simfitULS16 <- fitULS(simrep16)

saveRDS(object = simfitULS16,
        file = "simfitULS16.rds")

set.seed(6102)
simfitULS17 <- fitULS(simrep17)

saveRDS(object = simfitULS17,
        file = "simfitULS17.rds")

set.seed(6102)
simfitULS18 <- fitULS(simrep18)

saveRDS(object = simfitULS18,
        file = "simfitULS18.rds")


set.seed(6102)
simfitULS19 <- fitULS(simrep19)

saveRDS(object = simfitULS19,
        file = "simfitULS19.rds")

set.seed(6102)
simfitULS20 <- fitULS(simrep20)

saveRDS(object = simfitULS20,
        file = "simfitULS20.rds")


