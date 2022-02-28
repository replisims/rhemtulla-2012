View(rhemcon)
sim_plot <- ggplot2::ggplot(data = sim_coef_df_filtered) +
  ggplot2::geom_point(ggplot2::aes(x = cat, y = lambda_03 ))

aggregate_coef <- sim_coef_df %>% 
  dplyr::select(`f1=~y1`,`f2=~y6`, `f1=~y5`, `f2=~y10`, N, sym, cat, dist) %>% rowwise() %>% 
  dplyr::mutate(lambda_03 = mean(c(`f1=~y1`, `f2=~y6`), na.rm = TRUE)) %>%   
  dplyr::group_by(N, cat, sym, dist) %>% mutate(lambda_03_bar = mean(lambda_03, na.rm = TRUE)) %>% 
  mutate(bias_03 = lambda_03_bar - 0.3)

  
  glimpse(aggregate_coef)
  
  
  sim_plot <- ggplot2::ggplot(data = aggregate_coef) +
    ggplot2::geom_point(ggplot2::aes(x = cat, y = bias_03 ))
  
  
  model_table <- lavaanify(model = model_sim)
  sim_model1
  
  model_table_alt <- lavParTable(model = model_sim)
  
  
  plotDist(bindDist(skewness = 2, kurtosis = 7))
  1:6 %>% map(~hist(gen_data[,.x]))
  