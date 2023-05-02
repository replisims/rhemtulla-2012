sim_reps[[6]]

sim_reps4[[1]] %>% group_by(cat, sym, dist) %>% 
  


sim_data_tbl <- sim_reps4[[1]] %>% tibble

sim_data_tbl %>% 
  filter(cat == 2 & dist == "non-normal") %>% 
  select(-id) %>% 
  summarise(count)


sim_data_tbl_long <- sim_data_tbl%>% group_by(cat, sym, dist) %>% unnest(cols = cat_data)

sim_data_tbl_long %>% summarize(across(.cols = y1:y10, .fns = table))

sim_data_tbl_long %>% head()


test_tbl <- tibble(a = 1:10,
                   b = 2:11,
                   c = 3:12)

unlist(test_tbl)

sim_data_dump <- sim_data_tbl %>% mutate(cat_data_dump = list(unlist(cat_data)))

sim_data_dump %>% group_by(cat, sym, dist) %>% hist(unlist(cat_data_dump))

sim_data_dump