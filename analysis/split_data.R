
# Split data --------------------------------------------------------------

1:20 %>% map(~{subset <- simreps[(1+(50*(.x-1))):(50*.x)]
  saveRDS(subset, file = paste0("simrep",.x, ".rds"))})
